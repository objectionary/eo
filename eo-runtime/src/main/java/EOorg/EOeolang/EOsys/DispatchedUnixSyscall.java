package EOorg.EOeolang.EOsys;

import org.eolang.Phi;

public final class DispatchedUnixSyscall implements DispatchedNativeMethod {
    private final DispatchedNativeMethod origin;

    DispatchedUnixSyscall(final String name) {
        this.origin = new DispatchedNativeDefault(CStdLib.CSTDLIB, name);
    }

    @Override
    public int call(Phi... params) {
        return this.origin.call(params);
    }
}
