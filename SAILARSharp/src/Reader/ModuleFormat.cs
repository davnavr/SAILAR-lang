namespace SAILARSharp.Reader {
    using System;
    using SAILARSharp.Interop;

    public unsafe sealed class ModuleFormat : IDisposable {
        private OpaqueModuleFormat* format;
        private bool disposed = false;
        private readonly object theLock = new();

        public ModuleFormat(OpaqueModuleFormat* format) {
            this.format = format;
        }

        public byte GetMajorFormatVersion() => SAILAR.GetModuleFormatMajorVersion(format);

        public byte GetMinorFormatVersion() => SAILAR.GetModuleFormatMinorVersion(format);

        public byte GetIntegerByteSize() => SAILAR.GetModuleFormatIntegerByteSize(format);

        public void Dispose() {
            lock (theLock) {
                if (!disposed) {
                    GC.SuppressFinalize(this);
                    SAILAR.DisposeModuleFormat(format);
                    format = null;
                    disposed = true;
                }
            }
        }

        ~ModuleFormat() {
            Dispose();
        }
    }
}
