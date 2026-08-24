/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Stat64FuncCall}.
 * @since 0.74.1
 */
@ExtendWith(MktmpResolver.class)
final class Stat64FuncCallTest {

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void reportsTheFileSizeNotATimestamp(@Mktmp final Path temp) throws IOException {
        final byte[] content = "queried by a distinct, visibly-sized payload".getBytes();
        final Path file = temp.resolve("sized.txt");
        Files.write(file, content);
        final Phi result = new Stat64FuncCall(Phi.Φ.take("win32").copy()).make(
            new Data.ToPhi(file.toString())
        );
        MatcherAssert.assertThat(
            "The reported size must be the file's actual byte length, not an epoch timestamp misread from the following field",
            new Dataized(result.take("output").take("size")).asNumber().longValue(),
            Matchers.equalTo((long) content.length)
        );
    }
}
