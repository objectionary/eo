/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOtxt; // NOPMD

import java.nio.charset.StandardCharsets;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.PhVoid;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * String buffer.
 * @since 0.41.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "string-buffer")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOstring_buffer extends PhDefault implements Atom {
    
    /**
     * Ctor.
     */
    public EOstring_buffer() {
        this.add("m", new PhVoid("m"));
        this.add("content", new PhVoid("content"));
    }

    @Override
    public Phi lambda() {
        final String content = new Dataized(this.take("content")).asString();
        
        // For now, just return the content without using memory
        // TODO: implement memory management with m.resized
        return new Data.ToPhi(content);
    }
}