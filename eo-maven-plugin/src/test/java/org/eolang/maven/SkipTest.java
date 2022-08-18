package org.eolang.maven;

import com.yegor256.tojos.Json;
import com.yegor256.tojos.MonoTojos;
import org.cactoos.io.InputOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class SkipTest {

    @Test
    public void testExecutedPullMojo(@TempDir final Path temp) {
        final Path target = temp.resolve("target");
        executePullMojo(temp, target, false);
        MatcherAssert.assertThat(
                Files.exists(
                        target.resolve(
                                String.format(
                                        "%s/org/eolang/io/stdout.eo",
                                        PullMojo.DIR
                                )
                        )
                ),
                Matchers.is(true)
        );
    }

    @Test
    public void testSkippedPullMojo(@TempDir final Path temp) {
        final Path target = temp.resolve("target");
        executePullMojo(temp, target, true);
        MatcherAssert.assertThat(
                !Files.exists(
                        target.resolve(
                                String.format(
                                        "%s/org/eolang/io/stdout.eo",
                                        PullMojo.DIR
                                )
                        )
                ),
                Matchers.is(true)
        );
    }

    private void executePullMojo(@TempDir final Path temp, final Path target, final boolean skip) {
        final Path foreign = temp.resolve("eo-foreign.json");
        new MonoTojos(new Json(foreign))
                .add("org.eolang.io.stdout")
                .set(AssembleMojo.ATTR_SCOPE, "compile")
                .set(AssembleMojo.ATTR_VERSION, "*.*.*");
        new Moja<>(PullMojo.class)
                .with("targetDir", target.toFile())
                .with("foreign", foreign.toFile())
                .with("foreignFormat", "json")
                .with("skip", skip)
                .with(
                        "objectionary",
                        (Objectionary) input -> new InputOf("[] > hello\n")
                )
                .execute();
    }

    @Test
    public void testSkippedCopyMojo(@TempDir final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        executeCopyMojo(temp, classes, true);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
                !Files.exists(out),
                Matchers.is(true)
        );
    }

    @Test
    public void testExecutedCopyMojo(@TempDir final Path temp) throws IOException {
        final Path classes = temp.resolve("classes");
        executeCopyMojo(temp, classes, false);
        final Path out = classes.resolve("EO-SOURCES/foo/main.eo");
        MatcherAssert.assertThat(
                Files.exists(out),
                Matchers.is(true)
        );
    }

    private void executeCopyMojo(@TempDir final Path temp, final Path classes, final boolean skip) throws IOException {
        final Path src = temp.resolve("src");
        new Save(
                "+rt foo:0.0.0\n\n[args] > main\n  \"0.0.0\" > @\n",
                src.resolve("foo/main.eo")
        ).save();
        final String ver = "1.1.1";
        new Moja<>(CopyMojo.class)
                .with("sourcesDir", src.toFile())
                .with("outputDir", classes.toFile())
                .with("version", ver)
                .with("skip", skip)
                .execute();
    }
}
