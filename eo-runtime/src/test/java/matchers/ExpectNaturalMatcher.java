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

/*
 * @checkstyle PackageNameCheck (10 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package matchers;

import org.eolang.Dataized;
import org.eolang.ExAbstract;
import org.eolang.Phi;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;

/**
 * Matcher to check if dataizing a Phi throws an error
 * with a message indicating that an attribute must be a natural.
 *
 * @since 0.52
 */
public final class ExpectNaturalMatcher extends BaseMatcher<Phi> {

    @Override
    public boolean matches(final Object item) {
        boolean matches = false;
        try {
            new Dataized((Phi) item).take();
        } catch (final ExAbstract ex) {
            matches = ex
                .getMessage()
                .matches(".*the '.*' attribute \\(.*\\) must be greater or equal to zero.*");
        }
        return matches;
    }

    @Override
    public void describeTo(final Description description) {
        description.appendText("Transform Phi to Natural fails with correct error message");
    }

}
