/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
import java.net.URL;
import java.util.concurrent.TimeUnit;
import org.cactoos.text.Sticky;
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
        super(new Sticky(CommitHashesText::asText));
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
