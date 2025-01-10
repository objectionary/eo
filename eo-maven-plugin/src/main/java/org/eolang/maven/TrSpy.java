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
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StAfter;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;
import java.nio.file.Path;
import org.cactoos.Func;
import org.eolang.maven.footprint.Saved;

/**
 * Train that spies.
 *
 * @since 0.23
 */
final class TrSpy extends TrEnvelope {

    /**
     * Ctor.
     *
     * @param train Original one
     * @param dir The dir to save
     */
    TrSpy(final Train<Shift> train, final Func<XML, Path> dir) {
        super(
            new TrLambda(
                train,
                shift -> new StAfter(
                    shift,
                    new StLambda(
                        shift::uid,
                        (pos, xml) -> {
                            final String log = shift.uid().replaceAll("[^A-Za-z0-9]", "-");
                            new Saved(
                                xml.toString(),
                                dir.apply(xml).resolve(String.format("%02d-%s.xml", pos, log))
                            ).value();
                            if (Logger.isDebugEnabled(TrSpy.class)) {
                                Logger.debug(
                                    TrSpy.class, "Step #%d by %s:\n%s",
                                    pos, log, xml
                                );
                            }
                            return xml;
                        }
                    )
                )
            )
        );
    }
}
