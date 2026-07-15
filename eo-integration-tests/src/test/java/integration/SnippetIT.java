/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.jcabi.manifests.Manifests;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import com.yegor256.farea.RequisiteMatcher;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.regex.Pattern;
import org.cactoos.iterable.Mapped;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Integration test for simple snippets.
 * @since 0.57
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith(MktmpResolver.class)
final class SnippetIT {

    @ParameterizedTest
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    @ClasspathSource(value = "snippets", glob = "**.yaml")
    @SuppressWarnings("unchecked")
    void runsAllSnippets(final String yml, final @Mktmp Path temp) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(yml));
        Assumptions.assumeFalse(xtory.map().containsKey("skip"));
        final String[] log = {""};
        new Farea(temp).together(
            f -> {
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.files()
                    .file(String.format("src/main/eo/%s", xtory.map().get("file")))
                    .write(SnippetIT.eoBytes(xtory));
                f.dependencies().append(
                    "org.eolang", "eo-runtime", SnippetIT.runtimeVersion()
                );
                f.build().properties().set("directory", SnippetIT.targetDir(xtory));
                new EoSourceRun(f).exec(xtory.map().get("args"));
                log[0] = f.log().content();
                MatcherAssert.assertThat(
                    String.format("'%s' must build successfully, but it didn't", yml),
                    f.log(),
                    RequisiteMatcher.SUCCESS
                );
            }
        );
        MatcherAssert.assertThat(
            String.format("'%s' printed something wrong", yml),
            log[0],
            Matchers.allOf(
                new Mapped<>(
                    ptn -> Matchers.matchesPattern(
                        Pattern.compile(ptn, Pattern.DOTALL | Pattern.MULTILINE)
                    ),
                    (Iterable<String>) xtory.map().get("out")
                )
            )
        );
    }

    private static byte[] eoBytes(final Xtory xtory) {
        return String.format("%s%n", xtory.map().get("eo")).getBytes(StandardCharsets.UTF_8);
    }

    private static String targetDir(final Xtory xtory) {
        final String dir;
        if (xtory.map().containsKey("target")) {
            dir = xtory.map().get("target").toString();
        } else {
            dir = "target";
        }
        return dir;
    }

    private static String runtimeVersion() {
        return System.getProperty("eo.version", Manifests.read("EO-Version"));
    }
}
