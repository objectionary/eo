package org.eolang.maven;

import com.yegor256.Jaxec;
import java.io.IOException;
import java.nio.file.Path;
import org.eolang.maven.util.JniInfo;

public class CompiledCJniLib {
    /**
     * The single C source for building shared library.
     * @checkstyle MemberNameCheck (8 lines)
     */
    private final Path source;

    private final Path target;

    public CompiledCJniLib(final Path source, final Path target) {
        this.source = source;
        this.target = target;
    }

    public void build() throws IOException {
        // TODO: replace by paths and compile into class JniInfo
        final String cc = System.getenv("CC");
        // TODO: handle exception
        new Jaxec(
            cc,
            String.format("-I%s", JniInfo.COMMON_HEADER),
            String.format("-I%s", JniInfo.PLATFORM_SPECIFIC_HEADER),
            source.toString(),
            "-shared",
            "-o",
            target.toString()
        ).withCheck(false).execUnsafe();
    }
}
