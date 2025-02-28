/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonReader;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for {@link PullMojo}.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class PullMojoIT {
    @Test
    void ignoresPreviousMistakesAfterCorrection(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        "\n",
                        "+package org.eolang",
                        "",
                        "# In this program, we refer to the 'bar' object",
                        "# by mistake. The build should fail because of this,",
                        "# in particular its 'pull' step must fail.",
                        "[] > foo",
                        "  bar 42 > @",
                        ""
                    ).getBytes()
                );
                PullMojoIT.configureFarea(f);
                f.exec("eo:register", "eo:parse", "eo:shake", "eo:probe", "eo:pull");
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        "\n",
                        "+package org.eolang",
                        "",
                        "# Now, this program, doesn't refer to the 'bar' object",
                        "# which makes this program valid and it must compile.",
                        "[] > foo",
                        "  42 > @",
                        ""
                    ).getBytes()
                );
                f.exec("eo:register", "eo:parse", "eo:shake", "eo:probe", "eo:pull");
                MatcherAssert.assertThat(
                    "Foreign must not contain the a reference to an old object, but it doesn't",
                    PullMojoIT.ids(temp.resolve("target/eo-foreign.json")),
                    Matchers.not(Matchers.hasItem("org.eolang.dataized"))
                );
            }
        );
        MatcherAssert.assertThat(
            "necessary objects must were pulled",
            temp.resolve(
                String.format(
                    "target/eo/%s/org/eolang/number.eo",
                    PullMojo.DIR
                )
            ).toFile().exists(),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresPreviousChangesAfterCorrection(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        "\n",
                        "+package org.eolang",
                        "",
                        "# In this program, we refer to the 'String' object by mistake.",
                        "[] > foo",
                        "  \"Hello\" > @",
                        ""
                    ).getBytes()
                );
                PullMojoIT.configureFarea(f);
                f.exec("eo:register", "eo:parse", "eo:shake", "eo:probe", "eo:pull");
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        "\n",
                        "+package org.eolang",
                        "",
                        "# Now, this program, doesn't refer to the 'String' object",
                        "[] > foo",
                        "  42 > @",
                        ""
                    ).getBytes()
                );
                f.exec("eo:register", "eo:parse", "eo:shake", "eo:probe", "eo:pull");
                MatcherAssert.assertThat(
                    "Foreign must not contain the a reference to an old object",
                    PullMojoIT.ids(temp.resolve("target/eo-foreign.json")),
                    Matchers.not(Matchers.hasItem("org.eolang.string"))
                );
                MatcherAssert.assertThat(
                    "necessary objects must were pulled, but it didn't",
                    temp.resolve(
                        String.format(
                            "target/eo/%s/org/eolang/number.eo",
                            PullMojo.DIR
                        )
                    ).toFile().exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    "unnecessary objects were not removed",
                    temp.resolve(
                        String.format(
                            "target/eo/%s/org/eolang/string.eo",
                            PullMojo.DIR
                        )
                    ).toFile().exists(),
                    Matchers.is(false)
                );
            }
        );
    }

    private static void configureFarea(final Farea farea) throws IOException {
        farea.build()
            .plugins()
            .append(
                "org.eolang",
                "eo-maven-plugin",
                System.getProperty(
                    "eo.version",
                    Manifests.read("EO-Version")
                )
            )
            .configuration()
            .set("foreign", "${project.build.directory}/eo-foreign.json")
            .set("foreignFormat", "json");
    }

    private static List<String> ids(final Path path) throws IOException {
        final JsonReader reader = Json.createReader(
            Files.newBufferedReader(path)
        );
        final JsonArray json = reader.readArray();
        return IntStream.range(0, json.size())
            .mapToObj(json::getJsonObject)
            .map(obj -> obj.getString("id"))
            .collect(Collectors.toList());
    }
}
