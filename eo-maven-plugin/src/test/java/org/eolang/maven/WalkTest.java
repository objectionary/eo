/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Walk}.
 *
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class WalkTest {

    @Test
    void findsFiles(@Mktmp final Path temp) throws Exception {
        new HmBase(temp).save("", Paths.get("foo/hello/0.1/EObar/x.bin"));
        new HmBase(temp).save("", Paths.get("EOxxx/bar"));
        MatcherAssert.assertThat(
            "Walk is not iterable with more than 1 item, but it must be",
            new Walk(temp).includes(new ListOf<>("EO**/*")),
            Matchers.iterableWithSize(1)
        );
    }

}
