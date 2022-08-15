/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
package EOorg.EOeolang.EOtxt;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhMethod;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOsscanf}.
 *
 * @since 0.1
 */
public final class EOsscanfTest {

    @Test
    public void parseString() {
        final Phi format = new Data.ToPhi("Hello, %s");
        final Phi read = new Data.ToPhi("Hello, man");
        final Phi phi = new PhWith(
            new PhWith(
                new EOsscanf(Phi.Φ),
                "format",
                format
            ),
            "read",
            read
        );
        MatcherAssert.assertThat(
            new Dataized(new Dataized(phi).take(Phi[].class)[0]).take(String.class),
            Matchers.equalTo("man")
        );
    }

    @Test
    public void parseLong() {
        final Phi format = new Data.ToPhi("%d");
        final Phi read = new Data.ToPhi("1231231313");
        final Phi phi = new PhWith(
            new PhWith(
                new EOsscanf(Phi.Φ),
                "format",
                format
            ),
            "read",
            read
        );
        MatcherAssert.assertThat(
            new Dataized(
                new Dataized(phi).take(Phi[].class
                )[0])
                .take(Long.class),
            Matchers.equalTo(1231231313L)
        );
    }

    @Test
    public void parseDouble() {
        final Phi format = new Data.ToPhi("%f");
        final Phi read = new Data.ToPhi("1231231.22");
        final Phi phi = new PhWith(
            new PhWith(
                new EOsscanf(Phi.Φ),
                "format",
                format
            ),
            "read",
            read
        );
        MatcherAssert.assertThat(
            new Dataized(
                new Dataized(phi).take(Phi[].class
                )[0])
                .take(Double.class),
            Matchers.equalTo(1231231.22)
        );
    }

    @Test
    public void parseBoolean() {
        final Phi format = new Data.ToPhi("is %b");
        final Phi read = new Data.ToPhi("is true");
        final Phi phi = new PhWith(
            new PhWith(
                new EOsscanf(Phi.Φ),
                "format",
                format
            ),
            "read",
            read
        );
        MatcherAssert.assertThat(
            new Dataized(
                new Dataized(phi).take(Phi[].class
                )[0])
                .take(Boolean.class),
            Matchers.equalTo(true)
        );
    }

    @Test
    public void parseException() {
        final Phi format = new Data.ToPhi("is %l");
        final Phi read = new Data.ToPhi("is true");
        final Phi phi = new PhMethod(
            new PhWith(
                new PhWith(
                    new EOsscanf(Phi.Φ),
                    "format",
                    format
                ),
                "read",
                read
            ),
            "msg"
        );
        final ExFailure error = Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(phi).take(String.class)
        );
        MatcherAssert.assertThat(
            error.getMessage(),
            Matchers.containsString("Can't recognize format pattern: %l")
        );
    }

    @Test
    public void argumentException() {
        final Phi format = new Data.ToPhi("%123");
        final Phi read = new Data.ToPhi("1");
        final Phi phi = new PhMethod(
            new PhWith(
                new PhWith(
                    new EOsscanf(Phi.Φ),
                    "format",
                    format
                ),
                "read",
                read
            ),
            "msg"
        );
        final ExFailure error = Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(phi).take(String.class)
        );
        MatcherAssert.assertThat(
            error.getMessage(),
            Matchers.containsString("Can't recognize format pattern: %123")
        );
    }

}
