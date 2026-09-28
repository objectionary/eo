/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StAfter;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;

/**
 * Train that adds sheet names that were processed.
 *
 * @since 0.1
 */
final class TrStepped extends TrEnvelope {

    /**
     * Ctor.
     *
     * @param train Original train
     */
    TrStepped(final Train<Shift> train) {
        super(
            new TrLambda(
                train,
                shift -> new StAfter(
                    shift,
                    new StLambda(
                        shift::uid,
                        (pos, xml) -> new Sheets(shift.uid()).added(xml)
                    )
                )
            )
        );
    }
}
