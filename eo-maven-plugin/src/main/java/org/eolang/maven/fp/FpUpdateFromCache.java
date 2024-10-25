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
package org.eolang.maven.fp;

import com.jcabi.log.Logger;
import java.nio.file.Path;
import java.util.function.Supplier;
import org.cactoos.text.TextOf;

/**
 * Footprint that updates target from cache.
 * @since 0.41
 */
public final class FpUpdateFromCache extends FpEnvelope {
    /**
     * Ctor.
     * @param cache Lazy path to cache
     */
    public FpUpdateFromCache(final Supplier<Path> cache) {
        super(
            (source, target) -> {
                Logger.debug(
                    FpUpdateFromCache.class,
                    "Updating only target %[file]s from cache %[file]s",
                    target, source
                );
                return new Saved(new TextOf(cache.get()), target).value();
            }
        );
    }
}
