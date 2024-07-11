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
package EOorg.EOeolang.EOtxt;

import EOorg.EOeolang.EOerror;
import java.nio.charset.StandardCharsets;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOsprintf}.
 *
 * @since 0.39
 * @checkstyle TypeNameCheck (4 lines)
 */
final class EOsprintfTest {
    @Test
    void returnsValidFormattedString() {
        final Phi sprintf = new EOsprintf();
        sprintf.put(0, new Data.ToPhi("string %s, bytes %x, float %f, int %d, bool %b"));
        sprintf.put(
            1,
            new Data.ToPhi(
                new Phi[] {
                    new Data.ToPhi("Jeff"),
                    new Data.ToPhi("Jeff".getBytes(StandardCharsets.UTF_8)),
                    new Data.ToPhi(42.3),
                    new Data.ToPhi(14L),
                    new Data.ToPhi(false),
                }
            )
        );
        MatcherAssert.assertThat(
            "All types should be formatted successfully",
            new Dataized(sprintf).take(String.class),
            Matchers.equalTo(
                "string Jeff, bytes 4A-65-66-66, float 42.300000, int 14, bool false"
            )
        );
    }

    @Test
    void failsIfAmountOfArgumentsDoesNotMatch() {
        final Phi sprintf = new EOsprintf();
        sprintf.put(0, new Data.ToPhi("%s%s"));
        sprintf.put(1, new Data.ToPhi(new Phi[] {new Data.ToPhi("Hey")}));
        final Throwable exception = Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(sprintf).take(),
            "Dataization of sprintf should fail if formats < arguments"
        );
        MatcherAssert.assertThat(
            "The exception message should contain an info about arguments and format occurrences amount",
            exception.getMessage(),
            Matchers.containsString(
                "The amount of arguments 1 does not match the amount of format occurrences 2"
            )
        );
    }

    @Test
    void doesNotFailIfArgumentsMoreThanFormats() {
        final Phi sprintf = new EOsprintf();
        sprintf.put(0, new Data.ToPhi("%s"));
        sprintf.put(
            1,
            new Data.ToPhi(
                new Phi[] {
                    new Data.ToPhi("Hey"),
                    new Data.ToPhi("Hey"),
                }
            )
        );
        MatcherAssert.assertThat(
            "The second argument should be ignored",
            new Dataized(sprintf).take(String.class),
            Matchers.equalTo("Hey")
        );
    }

    @Test
    void failsOnUnsupportedFormat() {
        final Phi sprintf = new EOsprintf();
        sprintf.put(0, new Data.ToPhi("%o"));
        sprintf.put(1, Phi.Φ.take("org.eolang.tuple").take("empty"));
        Assertions.assertThrows(
            EOerror.ExError.class,
            () -> new Dataized(sprintf).take(),
            "Sprintf dataization should fail because of unsupported format %o"
        );
    }
}
