namespace SAILARSharp.Reader {
    using System;
    using SAILARSharp.Interop;

    public unsafe abstract class ModuleRecord : IDisposable {
        private OpaqueModuleRecord* record;
        private readonly object theLock = new();
        private bool disposed = false;

        // Prevent users from deriving their own module record subclasses.
        private protected ModuleRecord(OpaqueModuleRecord* record) {
            this.record = record;
        }

        public static ModuleRecord Create(OpaqueModuleRecord* record) {
            return SAILAR.GetModuleRecordType(record) switch
            {
                RecordType.Identifier => new IdentifierRecord(record),
                var invalid => throw new InvalidRecordTypeException(invalid),
            };
        }

        public OpaqueModuleRecord* Reference => record;

        private protected virtual void Cleanup() { }

        public void Dispose() {
            lock (theLock) {
                if (!disposed) {
                    GC.SuppressFinalize(this);
                    Cleanup();
                    SAILAR.DisposeModuleRecord(record);
                    record = null;
                    disposed = true;
                }
            }
        }

        ~ModuleRecord() => Dispose();
    }
}
