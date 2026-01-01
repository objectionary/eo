/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
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
