/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@code directory.tmpfile}.
 *
 * <p>A regular file passes the {@code exists} guard of the object, so the
 * open that follows is the one that fails. What it says about the failure is
 * what this test is about: the reason the OS gave, and not the number of it
 * (#9310). The check is POSIX-only, since the text of an OS error is the
 * one the host writes.</p>
 *
 * @since 0.0.0
 */
@ExtendWith(MktmpResolver.class)
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOdirectoryEOtmpfileTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void namesTheReasonItCannotCreateATmpfile(@Mktmp final Path temp) throws Exception {
        final Path regular = temp.resolve("regular.txt");
        Files.write(regular, "hello".getBytes(StandardCharsets.UTF_8));
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(regular.toString()));
        final Phi dir = Phi.Φ.take("directory").copy();
        dir.put(0, file);
        MatcherAssert.assertThat(
            "the failure must name the reason of the OS error and not its number, but it didnt",
            Assertions.assertThrows(
                ExFailure.class,
                () -> new Dataized(dir.take("tmpfile")).take(),
                "a temporary file inside a regular file must fail"
            ).getMessage(),
            Matchers.containsString("Not a directory")
        );
    }
}
