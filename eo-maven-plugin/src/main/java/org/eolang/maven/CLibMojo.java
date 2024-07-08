package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.commons.io.FilenameUtils;

@Mojo(
    name = "clib",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public class CLibMojo extends SafeMojo {
    /**
     * The directory containing sources for using C from EO (e.g. system calls).
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        property = "eo.clib",
        required = true,
        defaultValue = "${project.basedir}/src/main/c/eo/lib"
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File cEoLibDir;

    private final Path LIB_TARGET_DIR = Paths.get("Lib/native_clib");

    @Override
    void exec() throws IOException {
        Path target = this.targetDir.toPath().resolve(LIB_TARGET_DIR);
        Files.createDirectories(target);
        try (DirectoryStream<Path> files =
                 Files.newDirectoryStream(cEoLibDir.toPath(), "*.c")) {
            boolean contains = false;
            for (Path source : files) {
                contains = true;
                new NativeCLib(
                    source,
                    target.resolve(FilenameUtils.removeExtension(source.getFileName().toString()))
                ).build();
            }
            if (!contains) {
                throw new IllegalStateException("There are no C sources in the directory");
            }
        }
    }
}
