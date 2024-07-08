package org.eolang.maven;

import com.yegor256.Jaxec;
import java.io.IOException;
import java.nio.file.Path;
import org.eolang.maven.util.JniInfo;

public class NativeCLib {
    /**
     * The single C source for building shared library.
     * @checkstyle MemberNameCheck (8 lines)
     */
    private final Path source;

    private final Path target;

    /**
     * Ctor.
     * @param source Path to C source of native function.
     * @param target Path to the target file where the resulting native library will be compiled.
     */
    public NativeCLib(final Path source, final Path target) {
        this.source = source;
        this.target = target;
    }

    public void build() {
        final String cc = System.getenv("CC");
        try {
            new Jaxec(
                cc,
                String.format("-I%s", JniInfo.COMMON_HEADER),
                String.format("-I%s", JniInfo.PLATFORM_SPECIFIC_HEADER),
                source.toString(),
                "-shared",
                "-o",
                target.toString()
            ).exec();
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException(
                "An error occurred while compiling the source code of the C native library",
                e
            );
        }
    }
}
