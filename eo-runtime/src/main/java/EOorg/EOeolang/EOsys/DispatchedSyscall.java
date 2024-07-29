package EOorg.EOeolang.EOsys;

import org.eolang.Phi;

public interface DispatchedSyscall {
    int call(Phi... params);
}
