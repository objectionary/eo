/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link DepDirs}.
 *
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class DepDirsTest {

    @Test
    void findsDirs(@Mktmp final Path temp) throws IOException {
        new HmBase(temp).save("", Paths.get("a/b/c/f/test.txt"));
        new HmBase(temp).save("", Paths.get("a/b/f.txt"));
        new HmBase(temp).save("", Paths.get("test/f.txt"));
        new HmBase(temp).save("", Paths.get("a/g"));
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new DepDirs(temp),
            Matchers.contains(String.format("a%sb%1$sc%1$sf", File.separator))
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new DepDirs(temp),
            Matchers.iterableWithSize(1)
        );
    }

}
