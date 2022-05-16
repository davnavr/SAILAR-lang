namespace SAILARSharp.Writer {
    using System;
    using SAILARSharp.Interop;

    public unsafe sealed class ModuleBuilder : IDisposable {
        private OpaqueModuleBuilder* builder;
        private bool disposed = false;

        public ModuleBuilder() {
            throw new NotImplementedException();
        }

        public OpaqueModuleBuilder* Reference => builder;

        public void Dispose() {
            if (!disposed) {
                GC.SuppressFinalize(this);
                //SAILAR.DisposeModuleBuilder(builder);
                builder = null;
                disposed = true;
            }
        }
    }
}
