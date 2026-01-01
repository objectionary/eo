/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrFast;
import com.yegor256.xsline.TrLogged;
import com.yegor256.xsline.Train;
import java.util.logging.Level;

/**
 * Train full of logging and checking.
 * @since 0.51
 */
public final class TrFull extends TrEnvelope {
    /**
     * Ctor.
     */
    public TrFull() {
        this(new TrDefault<>());
    }

    /**
     * Ctor.
     * @param train Original
     */
    public TrFull(final Train<Shift> train) {
        super(
            new TrStepped(
                new TrFast(
                    new TrLogged(train, TrFull.class, Level.FINEST),
                    TrFull.class,
                    500L
                )
            )
        );
    }
}
