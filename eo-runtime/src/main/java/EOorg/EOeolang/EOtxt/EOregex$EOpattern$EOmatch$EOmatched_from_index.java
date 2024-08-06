/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang.EOtxt;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eolang.AtVoid;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Regex.pattern.match.matched-from-index.
 * @since 0.39.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "regex.pattern.match.matched-from-index")
public final class EOregex$EOpattern$EOmatch$EOmatched_from_index extends PhDefault
    implements Atom {
    /**
     * Ctor.
     */
    public EOregex$EOpattern$EOmatch$EOmatched_from_index() {
        this.add("position", new AtVoid("position"));
        this.add("start", new AtVoid("start"));
    }

    @Override
    public Phi lambda() throws Exception {
        final Phi match = this.take(Attr.RHO);
        final InputStream bais = new ByteArrayInputStream(
            new Dataized(match.take(Attr.RHO).take("serialized")).take()
        );
        final Matcher matcher = ((Pattern) new ObjectInputStream(bais).readObject()).matcher(
            new Dataized(match.take("txt")).asString()
        );
        final Phi start = this.take("start");
        final Double from = new Dataized(this.take("start")).asNumber();
        final boolean found = matcher.find(from.intValue());
        final Phi result;
        if (found) {
            result = match.take("matched");
            result.put("position", this.take("position"));
            result.put("start", start);
            result.put("from", new Data.ToPhi(matcher.start()));
            result.put("to", new Data.ToPhi(matcher.end()));
            final Phi[] groups;
            if (matcher.groupCount() > 0) {
                groups = new Phi[matcher.groupCount() + 1];
                for (int idx = 0; idx < groups.length; ++idx) {
                    groups[idx] = new Data.ToPhi(matcher.group(idx));
                }
            } else {
                groups = new Phi[] {new Data.ToPhi(matcher.group())};
            }
            result.put("groups", new Data.ToPhi(groups));
        } else {
            result = match.take("not-matched");
            result.put("position", this.take("position"));
        }
        return result;
    }
}
