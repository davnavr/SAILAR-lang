namespace SAILSharp.Reader {
    using System;
    using SAILSharp.Interop;

    public unsafe sealed class ModuleRecord : IDisposable {
        private OpaqueModuleRecord* record;
        private bool disposed = false;

        public ModuleRecord(OpaqueModuleRecord* record) {
            this.record = record;
        }

        public OpaqueModuleRecord* Reference => record;

        public void Dispose() {
            if (!disposed) {
                GC.SuppressFinalize(this);
                SAILAR.DisposeModuleRecord(record);
                record = null;
                disposed = true;
            }
        }
    }
}
