/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.SecureRandom;
import java.util.Set;
import java.util.UUID;
import org.eolang.cache.Saved;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.aggregator.ArgumentsAccessor;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link MjUnplace}.
 *
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
final class MjUnplaceTest {

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void cleansAllTheFiles(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Path clazz = MjUnplaceTest.placed(temp, maven, "class");
        final Path text = MjUnplaceTest.placed(temp, maven, "txt");
        final Path bin = MjUnplaceTest.placed(temp, maven, "so");
        MatcherAssert.assertThat(
            "After executing UnplaceMojo, all the placed files must be removed",
            maven
                .execute(MjUnplace.class)
                .result(),
            Matchers.allOf(
                Matchers.not(Matchers.hasValue(clazz)),
                Matchers.not(Matchers.hasValue(text)),
                Matchers.not(Matchers.hasValue(bin))
            )
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void keepsClasses(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).with("keepBinaries", Set.of("**/*.class"));
        final Path clazz = MjUnplaceTest.placed(temp, maven, "class");
        final Path text = MjUnplaceTest.placed(clazz, maven, "txt");
        MatcherAssert.assertThat(
            "UnplaceMojo must keep .class files and remove .txt file",
            maven
                .execute(MjUnplace.class)
                .result(),
            Matchers.allOf(
                Matchers.hasValue(clazz),
                Matchers.not(Matchers.hasValue(text))
            )
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void updatesPlacedTojosFile(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Path file = MjUnplaceTest.placed(temp, maven, "bat");
        MatcherAssert.assertThat(
            "Tojo must be marked as unplaced",
            maven.execute(MjUnplace.class).placed().find(file).get().unplaced(),
            Matchers.is(true)
        );
    }

    @Test
    void deletesAllEmptyDirectories(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MjUnplaceTest.placed(temp, maven, Paths.get("org/eolang/index.html"));
        MjUnplaceTest.placed(temp, maven, Paths.get("org/styles.css"));
        MatcherAssert.assertThat(
            "UnplaceMojo must delete all empty directories",
            maven.execute(MjUnplace.class).result(),
            Matchers.allOf(
                Matchers.not(Matchers.hasKey("target/classes/org/eolang")),
                Matchers.not(Matchers.hasKey("target/classes/org")),
                Matchers.hasKey("target/classes")
            )
        );
    }

    @ParameterizedTest
    @CsvSource({
        "class,true,true,false,false", "txt,true,true,false,false",
        "class,true,false,false,false", "txt,true,false,false,false",
        "class,false,false,false,false", "txt,false,false,false,false",
        "class,true,false,true,false", "txt,true,false,true,false",
        "class,false,false,false,true", "txt,false,false,false,true"
    })
    void unplacesOnlyCopiedBinaries(
        final ArgumentsAccessor args, @Mktmp final Path temp
    ) throws IOException {
        final boolean exists = args.getBoolean(1);
        final boolean rewrite = args.getBoolean(3);
        final FakeMaven maven = new FakeMaven(temp).with("rewrite", rewrite);
        final String binary = String.format("org/example/file.%s", args.getString(0));
        final byte[] original = {1, 2};
        final byte[] dependency;
        if (args.getBoolean(2)) {
            dependency = original;
        } else {
            dependency = new byte[]{3, 4};
        }
        new Saved(
            dependency,
            new Subdir(maven.targetPath(), "resolve").path()
                .resolve("foo/hello/-/0.1").resolve(binary)
        ).value();
        final Path target = maven.classesPath().resolve(binary);
        if (exists) {
            new Saved(original, target).value();
        }
        maven.execute(MjPlace.class);
        if (args.getBoolean(4)) {
            maven.execute(MjPlace.class);
        }
        final boolean owned = maven.placed().find(target).isPresent();
        final byte[] placed = Files.readAllBytes(target);
        maven.execute(MjUnplace.class);
        final boolean retained = Files.exists(target);
        final byte[] remaining;
        if (retained) {
            remaining = Files.readAllBytes(target);
        } else {
            remaining = new byte[0];
        }
        final boolean copied = !exists || rewrite;
        Assertions.assertAll(
            () -> Assertions.assertEquals(copied, owned, "Only copied binaries must be owned"),
            () -> Assertions.assertEquals(
                !copied, retained, "Only copied binaries must be removed"
            ),
            () -> {
                if (copied) {
                    Assertions.assertArrayEquals(dependency, placed, "The binary must be copied");
                } else {
                    Assertions.assertArrayEquals(original, placed, "The binary must be skipped");
                    Assertions.assertArrayEquals(original, remaining, "Unowned bytes must survive");
                }
            }
        );
    }

    private static Path placed(
        final Path temp, final FakeMaven maven, final String ext
    ) throws IOException {
        return MjUnplaceTest.placed(
            temp,
            maven,
            Paths.get(
                String.format("a/b/c/%d_foo.%s", new SecureRandom().nextInt(), ext)
            )
        );
    }

    private static Path placed(
        final Path temp, final FakeMaven maven, final Path relative
    ) throws IOException {
        final Path file = new Saved(
            UUID.randomUUID().toString(),
            maven.classesPath().resolve(relative)
        ).value();
        maven.placed().placeClass(
            file,
            temp.relativize(file).toString(),
            "eo-lib"
        );
        return file;
    }
}
