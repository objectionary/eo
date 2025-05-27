/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Integration test for EO snippets in README.md
 * @since 0.56.3
 */
final class ReadmeSnippetsIT {

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
                    .file(String.format("src/main/eo/%s.eo", "snippet"))
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
                new EoMavenPlugin(f)
                    .appended()
                    .execution("compile")
                    .phase("generate-sources")
                    .goals("register", "compile", "transpile")
                    .configuration()
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("skipLinting", Boolean.TRUE.toString());
                f.build()
                    .plugins()
                    .append("org.codehaus.mojo", "exec-maven-plugin", "3.1.1")
                    .execution("run")
                    .phase("test")
                    .goals("java")
                    .configuration()
                    .set("mainClass", "org.eolang.Main")
                    .set("arguments", "app");
                f.exec("clean", "test");
                // match content
//                    System.out.println(f.log().content());
            }
        );
    }

    private static Stream<Arguments> snippets() throws IOException {
        final Stream.Builder<Arguments> result = Stream.builder();
        final String content = Files.readString(
            Paths.get("").toAbsolutePath().getParent().resolve("README.md")
        );
        final Pattern pattern = Pattern.compile("(?ms)```eo\\s+(.*?)```");
        final Matcher matcher = pattern.matcher(content);
        while (matcher.find()) {
            result.add(Arguments.of(matcher.group(1)));
        }
        return result.build();
    }
}
