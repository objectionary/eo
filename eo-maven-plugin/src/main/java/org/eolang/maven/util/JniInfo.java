package org.eolang.maven.util;

import java.util.Locale;
import java.util.Map;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;

public final class JniInfo {
    /**
     * OS name.
     */
    private static final String OS_NAME = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);

    private static final Map<String, String> OS_TO_DIRECTORY = new MapOf<>(
        new MapEntry<>("linux", "linux"),
        new MapEntry<>("mac", "darwin"),
        new MapEntry<>("windows", "windows")
    );

    private static final String JAVA_HOME = System.getProperty("java.home");

    public static final String COMMON_HEADER = String.format("%s/include", JAVA_HOME);

    public static final String PLATFORM_SPECIFIC_HEADER = String.format(
        "%s/%s", COMMON_HEADER, specificIncludeDirName()
    );

    private JniInfo() {}

    /**
     * The name of the directory that contains the platform-specific C header for JNI.
     * @link <a href="https://mail.openjdk.org/pipermail/discuss/2011-June/001918.html">Where to find jni_md.h</a>
     */
    private static String specificIncludeDirName() {
        for (Map.Entry<String, String> entry : OS_TO_DIRECTORY.entrySet()) {
            if (JniInfo.OS_NAME.contains(entry.getKey())) {
                return entry.getValue();
            }
        }
        throw new IllegalStateException("Unavailable OS for native C standard lib usage");
    }
}
