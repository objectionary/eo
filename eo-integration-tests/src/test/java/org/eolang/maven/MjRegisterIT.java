/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import com.yegor256.tojos.MnCsv;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.TjSmart;
import com.yegor256.tojos.TjSynchronized;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for eo-maven-plugin:register goal.
 * @since 0.52
 */
@SuppressWarnings(
    {
        "JTCOP.RuleAllTestsHaveProductionClass",
        "JTCOP.RuleNotContainsTestWord",
        "PMD.AvoidDuplicateLiterals"
    }
)
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjRegisterIT {

    @Test
    void removesOldPulledFiles(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                MjRegisterIT.run(
                    f,
                    new String[]{"  \"Pull\" > @", "  41 > @"},
                    "eo:register", "eo:parse", "eo:probe", "eo:pull"
                );
                f.exec("eo:register");
                MatcherAssert.assertThat(
                    "Old pulled files must were removed, but it didn't",
                    temp.resolve("target/eo/2-pull").toFile().exists(),
                    Matchers.is(false)
                );
            }
        );
    }

    @Test
    void removesOldResolvedFiles(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                MjRegisterIT.run(
                    f,
                    new String[]{"  \"Resolve\" > @", "  42 > @"},
                    "eo:register", "eo:parse", "eo:probe", "eo:pull", "eo:resolve"
                );
                f.exec("eo:register");
                MatcherAssert.assertThat(
                    "Old resolved files must were removed, but it didn't",
                    temp.resolve("target/eo/2-pull").toFile().exists(),
                    Matchers.is(false)
                );
            }
        );
    }

    @Test
    void removesOldForeignFile(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                MjRegisterIT.runForeign(f);
                final TjSmart foreign = MjRegisterIT.loadForeign(temp);
                MatcherAssert.assertThat(
                    "Foreign must contain only 3 references to objects, but it doesn't",
                    foreign.size(),
                    Matchers.equalTo(3)
                );
                MatcherAssert.assertThat(
                    "Foreign must contain refs to Number, Bytes, and current object",
                    MjRegisterIT.existences(foreign, "number", "bytes", "foo"),
                    Matchers.everyItem(Matchers.is(true))
                );
                MatcherAssert.assertThat(
                    "Foreign must not contain a reference to an old object",
                    foreign.select(tojo -> "string".equals(tojo.get("id"))).isEmpty(),
                    Matchers.is(true)
                );
            }
        );
    }

    @Test
    void removesUnnecessaryPulledObjects(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                MjRegisterIT.run(
                    f,
                    new String[]{"  \"Hello\" > @", "  42 > @"},
                    "eo:register", "eo:parse", "eo:probe", "eo:pull"
                );
                f.exec("eo:register", "eo:parse", "eo:probe", "eo:pull");
                MatcherAssert.assertThat(
                    "Necessary objects must were pulled",
                    temp.resolve("target/eo/2-pull/number.eo").toFile().exists(),
                    Matchers.is(true)
                );
                MatcherAssert.assertThat(
                    "Unnecessary objects were not removed",
                    temp.resolve("target/eo/2-pull/string.eo").toFile().exists(),
                    Matchers.is(false)
                );
            }
        );
    }

    private static void run(
        final Farea farea, final String[] bodies, final String... goals
    ) throws IOException {
        farea.clean();
        farea.files().file("src/main/eo/foo.eo").write(
            MjRegisterIT.program("# Foo.", "[] > foo", bodies[0])
        );
        new AppendedPlugin(farea).value();
        farea.exec(goals);
        farea.files().file("src/main/eo/foo.eo").write(
            MjRegisterIT.program("# Foo.", "[] > foo", bodies[1])
        );
    }

    private static void runForeign(final Farea farea) throws IOException {
        farea.clean();
        farea.files().file("src/main/eo/foo.eo").write(
            MjRegisterIT.program(
                "+rt jvm org.eolang:eo-runtime:0.25.0",
                "",
                "# In this program, we refer to the 'String'",
                "[] > foo",
                "  \"42\" > @"
            )
        );
        new AppendedPlugin(farea).value();
        farea.exec("eo:register", "eo:parse", "eo:probe", "eo:pull", "eo:resolve");
        farea.files().file("src/main/eo/foo.eo").write(
            MjRegisterIT.program(
                "# In this program, we refer to the 'Number'.",
                "[] > foo",
                "  42 > @"
            )
        );
        farea.exec("eo:register", "eo:parse", "eo:probe", "eo:pull");
    }

    private static byte[] program(final String... lines) {
        return String.join(System.lineSeparator(), lines).getBytes(StandardCharsets.UTF_8);
    }

    private static TjSmart loadForeign(final Path temp) {
        return new TjSmart(
            new TjSynchronized(
                new TjCached(
                    new TjDefault(
                        new MnCsv(temp.resolve("target/eo-foreign.json"))
                    )
                )
            )
        );
    }

    private static java.util.List<Boolean> existences(
        final TjSmart foreign, final String... ids
    ) {
        return Stream.of(ids)
            .map(id -> foreign.getById(id).exists("id"))
            .collect(Collectors.toList());
    }
}
