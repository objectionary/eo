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

/*
 * @checkstyle PackageNameCheck (10 lines)
 */
package EOorg.EOeolang;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOstring$EOeq}.
 * {@link EOorg.EOeolang.EOio.EOstdin} is the generated class. This is the reason
 * why we disable jtcop check.
 *
 * @since 0.17
 * @checkstyle TypeNameCheck (4 lines)
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class EOstringTest {

    @Test
    void comparesTwoEqualStrings() {
        final String txt = "Hello, друг!";
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhMethod(new Data.ToPhi(txt), "eq"),
                    0, new Data.ToPhi(txt)
                )
            ).take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @Test
    void comparesTwoDifferentStrings() {
        MatcherAssert.assertThat(
            new Dataized(
                new PhWith(
                    new PhMethod(new Data.ToPhi("Hello, друг!"), "eq"),
                    0, new Data.ToPhi("Hello, world!")
                )
            ).take(Boolean.class),
            Matchers.equalTo(false)
        );
    }

}
