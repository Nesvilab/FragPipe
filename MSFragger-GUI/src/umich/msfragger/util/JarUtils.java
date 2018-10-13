package umich.msfragger.util;

import static umich.msfragger.params.ThisAppProps.JAR_FILE_AS_RESOURCE_EXT;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import umich.msfragger.params.ThisAppProps;

public class JarUtils {
    private JarUtils() {}

    /**
     * Unpack and possibly rename a file from this jar to a temp dir.
     * @throws IOException
     */
    public static Path unpackFromJar(String resourceLocation, boolean randomizeName,
        boolean scheduleForDeletion) throws IOException {

        return unpackFromJar(JarUtils.class, resourceLocation, randomizeName, scheduleForDeletion);
    }

    /**
     * Unpack and possibly rename a file from a jar where a class is loaded from to a temp dir.
     * @param clazz The jar from which to unpack is determined by this class.
     * @param resourceLocation Location of the resource within the jar.
     * @param randomizeName Make the name unique, even if there are similarly named files in the
     * temp directory.
     * @param scheduleForDeletion The file will be scheduled for deletion before JVM stops.
     * @return Path to unpacked file.
     * @throws IOException
     */
    public static Path unpackFromJar(Class<?> clazz, String resourceLocation,
        boolean randomizeName, boolean scheduleForDeletion) throws IOException {

        try (InputStream in = clazz.getResourceAsStream(resourceLocation)) {
            final String resourceNameDest = computeFinalResourceName(resourceLocation);

            Path tempFile = randomizeName
                ? Files.createTempFile("fragpipe-", "-" + resourceNameDest)
                : Paths.get(ThisAppProps.TEMP_DIR, resourceNameDest);

            Files.copy(in, tempFile, StandardCopyOption.REPLACE_EXISTING);
            if (scheduleForDeletion)
                tempFile.toFile().deleteOnExit();
            return tempFile;
        }

    }

    /**
     * Unpack and possibly rename a file from a jar where a class is loaded from to a temp dir.
     * @param clazz The jar from which to unpack is determined by this class.
     * @param resourceLocation Location of the resource within the jar.
     * @param locationInTemp Additional nested directories inside the system temp dir.
     * @param maintainRelLoc Recreate the original directory structure of the resource location
     * in temp. That will be on top of {@code locationInTemp}.
     * @param scheduleForDeletion The file will be scheduled for deletion before JVM stops.
     * @return Path to unpacked file.
     * @throws IOException
     */
    public static Path unpackFromJar(Class<?> clazz, String resourceLocation, Path locationInTemp,
        boolean maintainRelLoc, boolean scheduleForDeletion) throws IOException {

        try (InputStream in = clazz.getResourceAsStream(resourceLocation)) {
            final String resourceNameDest = computeFinalResourceName(resourceLocation);

            Path tempDir = Paths.get(ThisAppProps.TEMP_DIR);
            Path destDir = tempDir;
            if (locationInTemp != null)
                destDir = destDir.resolve(locationInTemp);
            if (maintainRelLoc) {
                String loc = computeResourceParent(resourceLocation);
                if (!StringUtils.isNullOrWhitespace(loc))
                    destDir = destDir.resolve(Paths.get(loc));
            }
            Path tempFile = destDir.resolve(resourceNameDest);

            if (Files.notExists(destDir)) {
                destDir = Files.createDirectories(destDir);
                if (scheduleForDeletion) {
                    destDir.toFile().deleteOnExit();
                }
            }

            Files.copy(in, tempFile, StandardCopyOption.REPLACE_EXISTING);
            if (scheduleForDeletion)
                tempFile.toFile().deleteOnExit();
            return tempFile;
        }
    }

    private static String computeResourceParent(String resourceLocation) {
        String s = StringUtils.upToLastChar(resourceLocation, '/', true);
        return s.startsWith("/") ? s.substring(1) : s;
    }

    private static String computeFinalResourceName(String resourceLocation) {
        final String resourceNameOrig = StringUtils.afterLastChar(resourceLocation, '/', false);
        return resourceNameOrig.toLowerCase().endsWith(JAR_FILE_AS_RESOURCE_EXT)
            ? StringUtils.upToLastDot(resourceLocation) + ".jar"
            : resourceNameOrig;
    }
}
