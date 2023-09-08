/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import java.nio.file.Path;
import java.util.Arrays;
import org.cactoos.iterable.Mapped;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * Asserting that path contains a files matching provided globs.
 * @since 0.31.0
 * @todo #1602:30min After introducing this class we are able to use
 *  it instead of {@link ContainsFile} because we can pass varargs into
 *  constructor. So, the next step will be exactly that - we will just remove
 *  ContainsFile class (inline it into ContainsFiles) because we can do it,
 *  since we don't use ContainsFile anymore and the code will become smaller and
 *  simpler without redundant class.
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleCorrectTestName"})
final class ContainsFiles extends TypeSafeMatcher<Path> {
    /**
     * Patterns.
     */
    private final String[] globs;

    /**
     * Ctor.
     * @param glbs Patterns
     */
    ContainsFiles(final String... glbs) {
        this.globs = Arrays.copyOf(glbs, glbs.length);
    }

    @Override
    public void describeTo(final Description description) {
        description.appendText(String.format("Matching globs: %s", Arrays.toString(this.globs)));
    }

    @Override
    public boolean matchesSafely(final Path path) {
        boolean matches = true;
        for (final ContainsFile matcher : new Mapped<>(ContainsFile::new, this.globs)) {
            if (!matcher.matchesSafely(path)) {
                matches = false;
                break;
            }
        }
        return matches;
    }
}
