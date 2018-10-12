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
     * @param scheduleForDeletion The file will be scheduled for deletion before JVM stops.
     * @return Path to unpacked file.
     * @throws IOException
     */
    public static Path unpackFromJar(Class<?> clazz, String resourceLocation, Path locationInTemp,
        boolean scheduleForDeletion) throws IOException {

        try (InputStream in = clazz.getResourceAsStream(resourceLocation)) {
            final String resourceNameDest = computeFinalResourceName(resourceLocation);

            Path tempDir = Paths.get(ThisAppProps.TEMP_DIR);
            Path destDir = tempDir.resolve(locationInTemp);
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

    private static String computeFinalResourceName(String resourceLocation) {
        final String resourceNameOrig = StringUtils.afterLastChar(resourceLocation, '/', false);
        return resourceNameOrig.toLowerCase().endsWith(JAR_FILE_AS_RESOURCE_EXT)
            ? StringUtils.upToLastDot(resourceLocation) + ".jar"
            : resourceNameOrig;
    }
}
