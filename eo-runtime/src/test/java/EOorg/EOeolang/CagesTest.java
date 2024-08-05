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
package EOorg.EOeolang;

import org.eolang.AtCompositeTest;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.PhFake;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Cages}.
 * @since 0.36.0
 */
final class CagesTest {
    @Test
    void initializesObjectForTheFirstTime() {
        final Phi phi = new PhFake();
        final int locator = Cages.INSTANCE.init(phi);
        Assertions.assertDoesNotThrow(
            () -> Cages.INSTANCE.get(locator),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void rencagesTheSameObject() {
        final Phi phi = new PhFake();
        Cages.INSTANCE.init(phi);
        Assertions.assertDoesNotThrow(
            () -> Cages.INSTANCE.init(phi),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void encagesObjectWithLocator() {
        final Phi first = new PhFake();
        final Phi second = new PhFake();
        final int locator = Cages.INSTANCE.init(first);
        Cages.INSTANCE.encage(locator, second);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            Cages.INSTANCE.get(locator).hashCode(),
            Matchers.equalTo(second.hashCode())
        );
    }

    @Test
    void failsToEncageObjectIfIsWasInitialized() {
        final Phi phi = new PhFake();
        Assertions.assertThrows(
            ExFailure.class,
            () -> Cages.INSTANCE.encage(phi.hashCode(), phi),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void failsToEncageObjectOfDifferentForma() {
        final int locator = Cages.INSTANCE.init(new PhFake());
        Assertions.assertThrows(
            ExFailure.class,
            () -> Cages.INSTANCE.encage(locator, new Data.ToPhi(5L)),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void failsToGetObjectIfWasNotInitialized() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> Cages.INSTANCE.get(new PhFake().hashCode()),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }
}
