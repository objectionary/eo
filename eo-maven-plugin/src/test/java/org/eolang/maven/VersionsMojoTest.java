package org.eolang.maven;

import org.cactoos.text.TextOf;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;

final public class VersionsMojoTest {
    @Test
    void replacesVersionsOk(@TempDir final Path tmp) throws Exception {
        final FakeMaven maven = new FakeMaven(tmp)
            .withProgram(
                "[] > main",
                "  QQ.io.stdout|0.26.0 > @",
                "    QQ.txt.sprintf|0.28.10",
                "      \"Hello world\""
            ).execute(new FakeMaven.Versions());
        String s = new TextOf(
            tmp.resolve("foo/x/main.eo")
        ).asString();
        System.out.println(s);
    }
}
