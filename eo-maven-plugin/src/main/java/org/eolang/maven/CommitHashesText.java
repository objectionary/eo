/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
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
     * Tags.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    /**
     * Constructor.
     */
    CommitHashesText() {
        this(CommitHashesText::asText);
    }

    /**
     * Constructor.
     * @param source Text source.
     */
    private CommitHashesText(final Text source) {
        super(new Synced(new Sticky(source)));
    }

    /**
     * Download from the URL and return the content.
     *
     * @return The body of the web page
     */
    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static String asText() throws Exception {
        String hashes;
        try {
            hashes = new TextOf(new URL(CommitHashesText.HOME)).asString();
        } catch (final java.net.UnknownHostException ex) {
            Logger.warn(
                CommitHashesText.class,
                "Can't load hashes: %[exception]s",
                ex
            );
            hashes = CommitHashesMap.FAKES;
        }
        return hashes;
    }
}
