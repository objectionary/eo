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
    }

    @Override
    public Phi lambda() {
        final Phi memory = this.take("m");
        return new StringBufferState(memory, "");
    }

    /**
     * String buffer state that can be chained with .with calls.
     */
    public static final class StringBufferState extends PhDefault implements Atom {
        
        /**
         * Memory block.
         */
        private final Phi memory;
        
        /**
         * Current content.
         */
        private final String content;
        
        /**
         * Ctor.
         * @param mem Memory block
         * @param cnt Current content
         */
        public StringBufferState(final Phi mem, final String cnt) {
            this.memory = mem;
            this.content = cnt;
        }
        
        @Override
        public Phi take(final String name) {
            if ("with".equals(name)) {
                return new WithMethod(this);
            }
            return super.take(name);
        }
        
        @Override
        public Phi lambda() {
            // Get the content as bytes
            final byte[] bytes = this.content.getBytes(StandardCharsets.UTF_8);
            
            // Resize memory to fit the content using m.resized
            final Phi resized = this.memory.take("resized").copy();
            resized.put("new-size", new Data.ToPhi(bytes.length));
            new Dataized(resized).take(); // Execute the resize
            
            // Write content to memory using m.write
            final Phi writer = this.memory.take("write").copy();
            writer.put("offset", new Data.ToPhi(0));
            writer.put("data", new Data.ToPhi(bytes));
            new Dataized(writer).take(); // Execute the write
            
            // Return the string content
            return new Data.ToPhi(this.content);
        }
    }
    
    /**
     * The 'with' method implementation.
     */
    public static final class WithMethod extends PhDefault implements Atom {
        
        /**
         * The string buffer state.
         */
        private final StringBufferState state;
        
        /**
         * Ctor.
         * @param buffer The buffer state
         */
        public WithMethod(final StringBufferState buffer) {
            this.state = buffer;
        }
        
        @Override
        public Phi lambda() {
            // Access the first positional argument (the string to append)
            final String newStr = new Dataized(this.take("0")).asString();
            final String newContent = this.state.content + newStr;
            return new StringBufferState(this.state.memory, newContent);
        }
    }
}