/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XMLDocument;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Settled}.
 *
 * @since 0.73.0
 */
final class SettledTest {

    @Test
    void settlesAChainInDispatchOrder() {
        final SettledTest.Chain chain = this.chain(6);
        MatcherAssert.assertThat(
            "a long chain must be settled in one ordered work-queue pass",
            new ArrayList<>(
                chain.dispatched.answers(new LinkedHashMap<>(0)).entrySet()
            ),
            Matchers.equalTo(
                new ArrayList<>(chain.expected.entrySet())
            )
        );
    }

    @Test
    void retriesAnOutOfOrderChainWithoutChangingLearningOrder() {
        final SettledTest.Chain chain = this.chain(6);
        final List<Site> reversed = new ArrayList<>(chain.sites);
        Collections.reverse(reversed);
        MatcherAssert.assertThat(
            "a dependency learned later must requeue the earlier dispatch",
            new ArrayList<>(
                this.dispatched(reversed, chain.xml).answers(new LinkedHashMap<>(0))
                    .entrySet()
            ),
            Matchers.equalTo(new ArrayList<>(chain.expected.entrySet()))
        );
    }

    @Test
    void keepsTheLegacyBytesWhileReducingPasses() {
        final SettledTest.Chain chain = this.chain(24);
        boolean equal = true;
        for (int run = 0; run < 5; ++run) {
            final SettledTest.Legacy baseline = this.legacy(chain);
            final Map<String, String> candidate = chain.dispatched.answers(
                new LinkedHashMap<>(0)
            );
            final byte[] expected = baseline.pairs.toString().getBytes(StandardCharsets.UTF_8);
            final byte[] actual = candidate.toString().getBytes(StandardCharsets.UTF_8);
            equal = equal && Arrays.equals(actual, expected)
                && baseline.passes == 25;
        }
        MatcherAssert.assertThat(
            "the work queue must preserve bytes and reduce dispatch passes",
            equal,
            Matchers.is(true)
        );
    }

    private Dispatched dispatched(final List<Site> sites, final String xml) {
        return new Dispatched(
            new XMLDocument(xml), sites, Collections.emptyMap(),
            Collections.emptyMap(), Collections.emptyMap(), Collections.emptyList()
        );
    }

    private SettledTest.Chain chain(final int length) {
        final StringBuilder xml = new StringBuilder(1024);
        xml.append(
            String.join(
                "", "<provides><type id='Φ.app.root'><attr name='step0' ",
                "type='Φ.target.0'/></type>"
            )
        );
        final List<Site> sites = new ArrayList<>(length);
        final Map<String, String> expected = new LinkedHashMap<>(length);
        for (int idx = 0; idx < length; ++idx) {
            final String made = String.format("Φ.app.d%d", idx);
            final String bearer = this.bearer(idx);
            if (idx > 0) {
                xml.append(
                    String.format(
                        "<type id='%s'><attr name='step%d' type='Φ.target.%d'/></type>",
                        String.format("Φ.target.%d", idx - 1), idx, idx
                    )
                );
            }
            xml.append(String.format("<type id='Φ.target.%d'/>", idx));
            sites.add(
                new Site(
                    new Xnav(
                        new XMLDocument(
                            String.format(
                                "<o base='.step%d' loc='%s'><o loc='%s'/></o>",
                                idx, made, bearer
                            )
                        ).inner()
                    ).element("o")
                )
            );
            expected.put(made, String.format("Φ.target.%d", idx));
        }
        xml.append("</provides>");
        return new SettledTest.Chain(
            this.dispatched(sites, xml.toString()), sites, expected, xml.toString()
        );
    }

    private String bearer(final int idx) {
        String found = "Φ.app.root";
        if (idx > 0) {
            found = String.format("Φ.app.d%d", idx - 1);
        }
        return found;
    }

    private SettledTest.Legacy legacy(final SettledTest.Chain chain) {
        final Map<String, String> pairs = new LinkedHashMap<>(0);
        Map<String, String> found;
        int passes = 0;
        do {
            found = this.legacyPass(chain, pairs);
            pairs.putAll(found);
            passes += 1;
        } while (!found.isEmpty());
        return new SettledTest.Legacy(pairs, passes);
    }

    private Map<String, String> legacyPass(
        final SettledTest.Chain chain, final Map<String, String> pairs
    ) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(
            new XMLDocument(chain.xml), names, Collections.emptyList()
        );
        final Map<String, Map<String, String>> bound = new Copied(
            new Bound(
                Collections.emptyMap(), Collections.emptyMap(),
                Collections.emptyMap(), pairs, owned
            ).all(),
            pairs,
            Collections.emptyList()
        ).all();
        final Filled filled = new Filled(
            pairs, owned, new Puts(bound, new Holders(bound, pairs).all()),
            Collections.emptyList()
        );
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Site dispatch : chain.sites) {
            final String made = dispatch.made();
            final String known = pairs.getOrDefault(made, "");
            if (known.isEmpty()) {
                final String bearer = dispatch.bearer();
                final String kept;
                if (bearer.isEmpty()) {
                    kept = filled.instead(known, made, made);
                } else {
                    kept = filled.instead(
                        owned.attribute(names.getOrDefault(bearer, bearer), dispatch.name()),
                        bearer, made
                    );
                }
                if (!kept.isEmpty() && !kept.equals(made) && !kept.equals(known)) {
                    found.put(made, kept);
                }
            }
        }
        return found;
    }

    private static final class Legacy {

        /**
         * The pairs learned by the legacy passes.
         */
        private final Map<String, String> pairs;

        /**
         * The number of legacy dispatch passes.
         */
        private final int passes;

        Legacy(final Map<String, String> pairs, final int passes) {
            this.pairs = pairs;
            this.passes = passes;
        }
    }

    /**
     * Synthetic dispatch chain for tests.
     *
     * @since 0.73.0
     */
    private static final class Chain {

        /**
         * The dispatch solver.
         */
        private final Dispatched dispatched;

        /**
         * The dispatches in source order.
         */
        private final List<Site> sites;

        /**
         * The expected ordered pairs.
         */
        private final Map<String, String> expected;

        /**
         * The provides XML used to build the solver.
         */
        private final String xml;

        Chain(
            final Dispatched dispatched, final List<Site> sites,
            final Map<String, String> expected, final String xml
        ) {
            this.dispatched = dispatched;
            this.sites = sites;
            this.expected = expected;
            this.xml = xml;
        }
    }
}
