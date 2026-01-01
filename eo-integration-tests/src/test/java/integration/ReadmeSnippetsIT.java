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
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Integration test for EO snippets in `README.md`.
 * @since 0.56.3
 * @todo #4679:30min Enable ReadmeSnippetsIT when it's possible. The test was
 *  disabled because because Attr interface was returned back and now these
 *  tests don't work anymore. These tests must be enabled when new EO release
 *  is made.
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class ReadmeSnippetsIT {

    @Disabled
    @Tag("snippets")
    @ParameterizedTest
    @ExtendWith(MktmpResolver.class)
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    @MethodSource("snippets")
    void validatesReadmeSnippets(final String snippet, @Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.files()
                    .file(String.format("src/main/eo/%s.eo", "app"))
                    .write(
                        String.format("%s\n", snippet).getBytes(StandardCharsets.UTF_8)
                    );
                f.dependencies()
                    .append(
                        "org.eolang",
                        "eo-runtime",
                        System.getProperty(
                            "eo.version",
                            Manifests.read("EO-Version")
                        )
                    );
                f.build()
                    .properties()
                    .set("directory", "target");
                new EoSourceRun(f).exec("app");
                f.exec("clean", "test");
                MatcherAssert.assertThat(
                    String.format(
                        "EO snippet was not been executed as expected:\n%s",
                        snippet
                    ),
                    f.log().content(),
                    Matchers.containsString("BUILD SUCCESS")
                );
            }
        );
    }

    /**
     * EO snippets from README.md file.
     * @return Stream of EO snippets
     * @throws IOException if I/O fails
     */
    private static Stream<Arguments> snippets() throws IOException {
        final Stream.Builder<Arguments> result = Stream.builder();
        final Matcher matcher = Pattern.compile("(?ms)```eo\\s+(.*?)```").matcher(
            Files.readString(
                Paths.get("").toAbsolutePath().getParent().resolve("README.md")
            )
        );
        while (matcher.find()) {
            result.add(Arguments.of(matcher.group(1)));
        }
        return result.build();
    }
}
