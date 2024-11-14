/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.rust;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.eolang.maven.BinarizeParseTest;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Names}.
 *
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
final class NamesTest {
    @Test
    void solvesSameHashes(@Mktmp final Path temp) {
        final Names dispatcher = new Names(temp.resolve("names"));
        final String one = "AaAaAa";
        final String two = "AaAaBB";
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            one.hashCode(),
            Matchers.equalTo(two.hashCode())
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            dispatcher.name(one),
            Matchers.not(
                dispatcher.name(two)
            )
        );
    }

    @Test
    void recoversNames(@Mktmp final Path temp) throws IOException {
        final String names = "names";
        final List<String> locations = IntStream.range(0, 1000)
            .mapToObj(String::valueOf)
            .collect(Collectors.toList());
        final Names before = new Names(temp.resolve(names));
        final Map<String, String> functions = locations.stream().collect(
            Collectors.toMap(loc -> loc, before::name)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            locations.size(),
            Matchers.equalTo(functions.size())
        );
        before.save();
        final Names after = new Names(temp.resolve(names));
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            before,
            Matchers.equalTo(after)
        );
    }
}
