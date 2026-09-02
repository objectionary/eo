/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.URL;
import java.util.concurrent.TimeUnit;
import org.cactoos.Text;
import org.cactoos.text.Sticky;
import org.cactoos.text.Synced;
import org.cactoos.text.TextEnvelope;
import org.cactoos.text.TextOf;

/**
 * Commit hashes table as text from objectionary.
 *
 * <p>This class serves the purpose of the global cache in order to avoid
 * downloading the list of tags multiple times from objectionary.</p>
 *
 * @since 0.29.6
 */
final class CommitHashesText extends TextEnvelope {

    /**
     * Fallback hashes.
     */
    private static final String FALLBACK = String.join(
        System.lineSeparator(),
        "5fe5ad8d21dbe418038fa4c86e096fb037f290a9 0.23.15",
        "15c85d7f8cffe15b0deba96e90bdac98a76293bb 0.23.17",
        "4b19944d86058e3c81e558340a3a13bc335a2b48 0.23.19",
        "0aa6875c40d099c3f670e93d4134b629566c5643 0.25.0",
        "ff32e9ff70c2b3be75982757f4b0607dc37b258a 0.25.5",
        "e0b783692ef749bb184244acb2401f551388a328 0.26.0",
        "cc554ab82909eebbfdacd8a840f9cf42a99b64cf 0.27.0",
        "00b60c7b2112cbad4e37ba96b162469a0e75f6df 0.27.2",
        "6a70071580e95aeac104b2e48293d3dfe0669973 0.28.0",
        "0c15066a2026cec69d613b709a301f1573f138ec 0.28.1",
        "9b883935257bd59d1ba36240f7e213d4890df7ca 0.28.10",
        "a7a4556bf1aa697324d805570f42d70affdddb75 0.28.14",
        "54d83d4b1d28075ee623d58fd742eaa529febd3d 0.28.2",
        "6c6269d1f9a1c81ffe641538f119fe4e12706cb3 0.28.4",
        "9c9352890b5d30e1b89c9147e7c95a90c9b8709f 0.28.5",
        "17f89293e5ae6115e9a0234b754b22918c11c602 0.28.6",
        "5f82cc1edffad67bf4ba816610191403eb18af5d 0.28.7",
        "be83d9adda4b7c9e670e625fe951c80f3ead4177 0.28.9"
    );

    /**
     * Constructor.
     */
    CommitHashesText() {
        this(CommitHashesText::fetch);
    }

    /**
     * Constructor.
     * @param source Text source, retried on its own before this class falls
     *  back to {@link CommitHashesText#FALLBACK}
     */
    CommitHashesText(final Text source) {
        super(new Synced(new Sticky(() -> CommitHashesText.safe(source))));
    }

    private static String safe(final Text source) throws Exception {
        String hashes;
        try {
            hashes = source.asString();
        } catch (final IOException ex) {
            Logger.warn(
                CommitHashesText.class,
                "Can't load hashes: %[exception]s",
                ex
            );
            hashes = CommitHashesText.FALLBACK;
        }
        return hashes;
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static String fetch() throws Exception {
        return new TextOf(new URL("https://home.objectionary.com/tags.txt")).asString();
    }
}
