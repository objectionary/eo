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
package org.eolang.maven.optimization;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import java.nio.file.Path;
import org.eolang.maven.Place;
import org.eolang.maven.Rel;
import org.eolang.maven.SpyTrain;

/**
 * Optimization that spies.
 * @since 0.28.12
 */
public final class OptSpy implements Optimization {

    /**
     * Where to track optimization steps.
     */
    private final Path target;

    /**
     * The main constructor.
     * @param target Where to track optimization steps.
     */
    public OptSpy(final Path target) {
        this.target = target;
    }

    @Override
    public XML apply(final XML xml) {
        final Place place = new Place(xml.xpath("/program/@name").get(0));
        final Path dir = place.make(this.target, "");
        Logger.debug(
            this, "Optimization steps will be tracked to %s",
            new Rel(dir)
        );
        return new OptTrain(new SpyTrain(OptTrain.DEFAULT_TRAIN, dir)).apply(xml);
    }
}
