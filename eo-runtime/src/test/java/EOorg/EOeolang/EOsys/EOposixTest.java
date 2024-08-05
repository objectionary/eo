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
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang.EOsys;

import EOorg.EOeolang.EOtuple$EOempty;
import java.lang.management.ManagementFactory;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOposix}.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
final class EOposixTest {
    @Test
    @DisabledOnOs(OS.WINDOWS)
    public void invokesGetpidCorrectly() {
        MatcherAssert.assertThat(
            "The \"getpid\" system call was expected to work correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
                        new EOposix(),
                        "name",
                        new Data.ToPhi("getpid")
                    ),
                    "args",
                    new EOtuple$EOempty()
                )
            ).take(Long.class),
            Matchers.equalTo(
                Long.parseLong(
                    ManagementFactory.getRuntimeMXBean()
                        .getName().split("@")[0]
                )
            )
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    public void invokesWriteCorrectly() {
        final String msg = "Hello, world!\n";
        final Phi args = new Data.ToPhi(
            new Phi[] {
                new Data.ToPhi(1L),
                new Data.ToPhi(msg),
                new Data.ToPhi(msg.length()),
            }
        );
        MatcherAssert.assertThat(
            "The \"getpid\" system call was expected to work correctly",
            new Dataized(
                new PhWith(
                    new PhWith(
                        new EOposix(),
                        "name",
                        new Data.ToPhi("write")
                    ),
                    "args",
                    args
                )
            ).take(Long.class),
            Matchers.equalTo(
                (long) msg.length()
            )
        );
    }
}
