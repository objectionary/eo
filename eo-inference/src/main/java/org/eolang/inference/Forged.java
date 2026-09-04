/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Which answers are rooted at a void only an atom fills.
 *
 * <p>An atom is written in Java and hands back an object the source names:
 * {@code [] > @ /Q.posix.return} says that running it gives back a
 * {@code Φ.posix.return}, and that formation has voids of its own. Java fills
 * them, one {@code put} per void, as the syscall comes back with its answer.
 * No caller of the program fills them, and there is nowhere a reader can be
 * sent to find out what goes in there, because the thing that puts it there is
 * not written in EO at all.</p>
 *
 * <p>Without this the answer for such an object reads the same as the answer
 * for a void the callers disagree about, and the two ask opposite things of a
 * reader. One says go and look at the call sites; the other says the call
 * sites will never tell you, read the atom. So the answers rooted at a void of
 * a formation some atom comes back with are stamped here, once the walk is
 * over, and {@link Band} gives them a colour of their own (#8352).</p>
 *
 * <p>The receiver of such a formation is left alone: whoever writes
 * {@code posix.return} fills the {@code ρ} themselves, in EO, and the callers
 * are exactly who a reader should be sent to. So is the void an atom comes
 * back with rather than fills — {@code [] > recovered /A} over {@code ? > value
 * /A?} hands back whatever it was given, and {@code Φ.recovered.value} belongs
 * to whoever copied the atom (#8348). Such a void is what a {@code returns}
 * cell holds, not what the row it names declares, and falls out of the walk
 * for nothing.</p>
 *
 * @since 0.71.0
 */
final class Forged {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Ctor.
     * @param provides The provides table, which says what every atom comes
     *  back with and which voids that formation declares
     */
    Forged(final XML provides) {
        this.given = provides;
    }

    /**
     * Stamp the answers rooted at a void only an atom fills.
     * @param told The answers, by the locator of the object
     * @return The same answers, with the ones an atom fills told apart
     */
    Map<String, Answer> marked(final Map<String, Answer> told) {
        final Collection<String> hollows = this.hollows();
        final Map<String, Answer> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Answer> object : told.entrySet()) {
            found.put(object.getKey(), Forged.stamped(object.getValue(), hollows));
        }
        return found;
    }

    private Collection<String> hollows() {
        final Collection<String> backs = new HashSet<>(new Returned(this.given).all().values());
        final Collection<String> found = new HashSet<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            if (backs.contains(new Noted(type).says("id"))) {
                found.addAll(Forged.filled(type));
            }
        }
        return found;
    }

    private static Collection<String> filled(final Xnav type) {
        return type.elements(Filter.withName("attr"))
            .filter(attr -> "true".equals(new Noted(attr).says("void")))
            .filter(attr -> !"ρ".equals(new Noted(attr).says("name")))
            .map(attr -> new Noted(attr).says("type"))
            .collect(Collectors.toList());
    }

    private static Answer stamped(final Answer answer, final Collection<String> hollows) {
        final Answer found;
        if (answer.rung() == 1 && answer.seen().isEmpty()
            && Forged.rooted(answer.where(), hollows)) {
            found = new Answer(answer.where(), answer.rung(), answer.seen(), true);
        } else {
            found = answer;
        }
        return found;
    }

    private static boolean rooted(final String locator, final Collection<String> hollows) {
        String walked = locator;
        while (!hollows.contains(walked)) {
            final int dot = walked.lastIndexOf('.');
            if (dot < 0) {
                break;
            }
            walked = walked.substring(0, dot);
        }
        return hollows.contains(walked);
    }
}
