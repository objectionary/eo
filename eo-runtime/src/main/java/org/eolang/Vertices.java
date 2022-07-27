/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
package org.eolang;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

/**
 * Collection of all vertices.
 *
 * The class is thread-safe.
 *
 * @since 0.18
 */
final class Vertices {

    /**
     * Numbers of just objects.
     */
    private final AtomicInteger count = new AtomicInteger();

    /**
     * All seen.
     */
    private final ConcurrentHashMap<String, Integer> seen =
        new ConcurrentHashMap<>(0);

    /**
     * Get the next one.
     * @return Next vertex available
     */
    public int next() {
        return this.seen.computeIfAbsent(
            String.format("next:%d", this.count.addAndGet(1)),
            key -> this.seen.size() + 1
        );
    }

    /**
     * Get the best suitable one or next.
     * @param obj The object to find
     * @return Next vertex available or previously registered
     */
    public int best(final Object obj) {
        final MessageDigest digest;
        try {
            digest = MessageDigest.getInstance("SHA-256");
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException(ex);
        }
        digest.update(
            String.format(
                "%s %s",
                obj.getClass().getName(),
                this.labelFrom(obj)
            ).getBytes()
        );
        final String hash = new String(digest.digest());
        return this.bestByInstanceOf(obj, hash);
    }

    /**
     * Get label from Object.
     * @param obj Label from
     * @return Label based on obj instance
     */
    private String labelFrom(final Object obj) {
        final String label = this.labelByInstanceOf(obj);
        if (Objects.equals(null, label)) {
            throw new IllegalArgumentException(
                String.format(
                    "Unknown type for vertex allocation: %s",
                    obj.getClass().getCanonicalName()
                )
            );
        }
        return label;
    }

    /**
     * Mustn't change this method access level.
     * @param obj Whose instance check
     * @return Label from <b>obj</b>
     */
    private String labelByInstanceOf(final Object obj) {
        String label = null;
        if (this.canToString(obj)) {
            label = obj.toString();
        } else if (obj instanceof Pattern) {
            label = Pattern.class.cast(obj).pattern();
        } else if (obj instanceof byte[]) {
            label = Arrays.toString(byte[].class.cast(obj));
        }
        return label;
    }

    /**
     * Checks <b>obj</b> for the presence of toString() method.
     * @param obj Whose instance check
     * @return Presence as boolean
     */
    private boolean canToString(final Object obj) {
        return this.numeric(obj) || this.nonNumeric(obj) || obj instanceof Boolean;
    }

    /**
     * Checks obj on an instance of non-numeric type.
     * @param obj Whose instance check
     * @return Result instanceof obj
     */
    private boolean nonNumeric(final Object obj) {
        return obj instanceof String || obj instanceof Character;
    }

    /**
     * Checks obj on an instance of numeric type.
     * @param obj Whose instance check
     * @return Result instanceof obj
     */
    private boolean numeric(final Object obj) {
        return obj instanceof Long || obj instanceof Double;
    }

    /**
     * Picking up best by an instance of passed object.
     * @param obj From the best
     * @param hash Of digest
     * @return Best based on obj instance
     */
    private int bestByInstanceOf(final Object obj, final String hash) {
        int best = this.next();
        if (!(obj instanceof Phi[])) {
            best = this.seen.computeIfAbsent(
                hash, key -> this.seen.size() + 1
            );
        }
        return best;
    }

}
