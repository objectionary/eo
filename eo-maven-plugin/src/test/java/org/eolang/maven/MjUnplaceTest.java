/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.SecureRandom;
import java.util.Set;
import java.util.UUID;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjUnplace}.
 *
 * @since 0.1
 * @checkstyle LocalFinalVariableNameCheck (100 lines)
 */
@ExtendWith(MktmpResolver.class)
final class MjUnplaceTest {
    @Test
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

    /**
     * Place file to the placed tojos file.
     * @param temp Temporary directory
     * @param maven Maven instance
     * @param ext File extension
     * @return Path to placed file
     */
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

    /**
     * Place file into the placed tojos file.
     * @param temp Temporary directory
     * @param maven Maven instance
     * @param relative Relative path to file
     * @return Path to placed file
     * @throws IOException If Fails to place
     */
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
