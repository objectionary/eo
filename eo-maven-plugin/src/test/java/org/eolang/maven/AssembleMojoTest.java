/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link AssembleMojo}.
 *
 * @since 0.1
 * @todo #1602:30min Make up how to get rid of excessive usage of
 *  {@code ParseMojo.DIR}, {@code ResolveMojo.DIR} and so on. It would be nice
 *  to replace them with corresponding classes, or something similar
 * @todo #1602:30min Refactor tests. Logic of AssembleMojo is to run several
 *  phases one-by-one in a loop. Nothing more. Everything else you are trying to
 *  check here is related to particular mojos (and we should check their
 *  behaviour in appropriate tests). In other words there are integration tests
 *  here. And, probably, it is not the best place for them.
 */
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
final class AssembleMojoTest {
    @Test
    void executesAllMojos(@Mktmp final Path temp) throws IOException {
        final Map<String, Path> result = new FakeMaven(temp)
            .withHelloWorld()
            .execute(AssembleMojo.class)
            .result();
        MatcherAssert.assertThat(result.size(), Matchers.greaterThan(0));
    }

    @Test
    void doesNotCreateDuplicatesAfterRestarting(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withHelloWorld();
        Assertions.assertEquals(
            maven.execute(AssembleMojo.class).result(),
            maven.execute(AssembleMojo.class).result(),
            "Restarting of AssemblyMojo should not change the result"
        );
    }
}
