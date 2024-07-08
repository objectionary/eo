package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
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

    private final Path LIB_TARGET_DIR = Paths.get("Lib/native_cstdlib");

    @Override
    void exec() throws IOException {
        Path target = this.targetDir.toPath().resolve(LIB_TARGET_DIR);
        new File(target.toString()).mkdirs();
        // TODO: check if there is no files in directory
        for (File source : this.cEoStdLibDir.listFiles()) {
            new CompiledCJniLib(
                source.toPath(),
                target.resolve(FilenameUtils.removeExtension(source.getName()))
            );
        }
    }
}
