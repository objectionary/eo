package org.eolang.maven;

import com.yegor256.Jaxec;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Map;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapOf;
import org.apache.commons.io.FilenameUtils;

@Mojo(
    name = "cstdlib",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public class CStdLibMojo extends SafeMojo {
    /**
     * The directory containing sources for using C standard library from EO (e.g. system calls).
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        property = "eo.cstdlib",
        required = true,
        defaultValue = "${project.basedir}/src/main/c/eo/stdlib"
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File cEoStdLibDir;

    /**
     * OS name.
     */
    private static final String OS_NAME = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);

    private static final Map<String, String> OsToDirectory = new MapOf<>(
        new MapEntry<>("linux", "linux"),
        new MapEntry<>("mac", "darwin"),
        new MapEntry<>("windows", "windows")
    );

    private final Path LIB_TARGET_DIR = Paths.get("Lib/native_cstdlib");

    @Override
    void exec() throws IOException {
        Path target = this.targetDir.toPath().resolve(LIB_TARGET_DIR);
        new File(target.toString()).mkdirs();
        // TODO: check if there is no files in directory
        for (File source : this.cEoStdLibDir.listFiles()) {
            // TODO: replace by paths
            final String cc = System.getenv("CC");
            final String home = System.getProperty("java.home");
            final String common = String.format("%s/include", home);
            final String specific = String.format("%s/%s", common, specificIncludeDirName());
            System.out.println(
                new Jaxec(
                    cc,
                    String.format("-I%s", common),
                    String.format("-I%s", specific),
                    source.getPath(),
                    "-shared",
                    "-o",
                    target.resolve(
                        FilenameUtils.removeExtension(source.getName())
                    ).toString()
                ).withCheck(false).execUnsafe()
            );
        }
    }

    /**
     * The name of the directory that contains the platform-specific C header for JNI.
     * @link <a href="https://mail.openjdk.org/pipermail/discuss/2011-June/001918.html">Where to find jni_md.h</a>
     */
    private String specificIncludeDirName() {
        for (Map.Entry<String, String> entry : OsToDirectory.entrySet()) {
            if (CStdLibMojo.OS_NAME.contains(entry.getKey())) {
                return entry.getValue();
            }
        }
        throw new IllegalStateException("Unavailable OS for native C standard lib usage");
    }
}
