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
// @checkstyle PackageNameCheck (1 line)
package EOorg.EOeolang.EOsys;

import java.util.Locale;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * Uname is-linux.
 *
 * @since 0.38
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "uname.is-linux")
public final class EOuname$EOis_linux extends PhDefault implements Atom {

    @Override
    public Phi lambda() throws Exception {
        return new Data.ToPhi(
            System.getProperty("os.name").toLowerCase(Locale.ENGLISH).contains("linux")
        );
    }

}
