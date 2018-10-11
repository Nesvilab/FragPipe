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
     * @param randomizeName
     * @param scheduleForDeletion
     * @return Path to unpacked file.
     * @throws IOException
     */
    public static Path unpackFromJar(Class<?> clazz, String resourceLocation,
        boolean randomizeName, boolean scheduleForDeletion) throws IOException {

        try (InputStream in = clazz.getResourceAsStream(resourceLocation)) {
            Path tempFile;
            final String resourceName = StringUtils.afterLastChar(resourceLocation, '/', false);
            String resourceNameMod = resourceName.toLowerCase().endsWith(JAR_FILE_AS_RESOURCE_EXT)
                ? StringUtils.upToLastDot(resourceLocation) + ".jar"
                : resourceName;

            if (randomizeName) {
                tempFile = Files.createTempFile("fragpipe-", "-" + resourceNameMod);
            } else {
                tempFile = Paths.get(ThisAppProps.TEMP_DIR, resourceNameMod);
            }
            Files.copy(in, tempFile, StandardCopyOption.REPLACE_EXISTING);
            if (scheduleForDeletion)
                tempFile.toFile().deleteOnExit();
            return tempFile;
        }
    }

}
