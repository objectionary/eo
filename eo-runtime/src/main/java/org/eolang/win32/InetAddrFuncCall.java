/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import java.nio.ByteBuffer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * The 'inet_addr' WS2_32 function call.
 * @see <a href="https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-inet_addr">here for details</a>
 * @since 0.40.0
 */
public final class InetAddrFuncCall implements Syscall {

    /**
     * Dotted-decimal IPv4 literal, e.g. "127.0.0.1".
     */
    private static final Pattern IPV4 = Pattern.compile(
        "(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})"
    );

    /**
     * Win32 object.
     */
    private final Phi win;

    /**
     * Ctor.
     * @param win Win32 object
     */
    public InetAddrFuncCall(final Phi win) {
        this.win = win;
    }

    @Override
    public Phi make(final Phi... params) {
        final Phi result = this.win.take("return").copy();
        final String address = new Dataized(params[0]).asString();
        final Matcher matcher = InetAddrFuncCall.IPV4.matcher(address);
        if (matcher.matches() && InetAddrFuncCall.octetsValid(matcher)) {
            final ByteBuffer buffer = ByteBuffer.allocate(4);
            for (int octet = 1; octet <= 4; ++octet) {
                buffer.put((byte) Integer.parseInt(matcher.group(octet)));
            }
            result.put(0, new Data.ToPhi(Integer.reverseBytes(buffer.getInt(0))));
        } else {
            Winsock.INSTANCE.WSASetLastError(Winsock.WSAEINVAL);
            result.put(0, new Data.ToPhi(-1));
        }
        result.put(1, new PhDefault());
        return result;
    }

    /**
     * Checks that every matched octet fits into a byte.
     * @param matcher Matcher already matched against {@link #IPV4}
     * @return True if all four octets are in range 0-255
     */
    private static boolean octetsValid(final Matcher matcher) {
        boolean valid = true;
        for (int octet = 1; octet <= 4; ++octet) {
            if (Integer.parseInt(matcher.group(octet)) > 255) {
                valid = false;
                break;
            }
        }
        return valid;
    }
}
