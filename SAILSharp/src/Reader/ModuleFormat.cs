namespace SAILSharp.Reader {
    using System;
    using SAILSharp.Interop;

    public unsafe sealed class ModuleFormat : IDisposable {
        private OpaqueModuleFormat* format;
        private bool disposed = false;

        public ModuleFormat(OpaqueModuleFormat* format) {
            this.format = format;
        }

        public byte GetMajorFormatVersion() => SAILAR.GetModuleFormatMajorVersion(format);

        public byte GetMinorFormatVersion() => SAILAR.GetModuleFormatMinorVersion(format);

        public byte GetIntegerByteSize() => SAILAR.GetModuleFormatIntegerByteSize(format);

        public void Dispose() {
            if (!disposed) {
                GC.SuppressFinalize(this);
                SAILAR.DisposeModuleFormat(format);
                format = null;
                disposed = true;
            }
        }

        ~ModuleFormat() {
            Dispose();
        }
    }
}
