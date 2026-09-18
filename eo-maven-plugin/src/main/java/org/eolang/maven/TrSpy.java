/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StAfter;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;
import java.nio.file.Path;

/**
 * Train that spies.
 *
 * <p>The directory is fixed for the whole train, because a document halfway
 * through it no longer carries the name of the object it came from: after
 * {@code classes.xsl} an XMIR holding two objects has two {@code class}
 * elements and no {@code /object/o/@name} at all, and deriving the directory
 * from such a document breaks the build, see #4370.</p>
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
    TrSpy(final Train<Shift> train, final Path dir) {
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
                                dir.resolve(String.format("%02d-%s.xml", pos, log))
                            ).value();
                            if (Logger.isDebugEnabled(TrSpy.class)) {
                                Logger.debug(
                                    TrSpy.class, "Step #%d by %s:%n%s",
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
