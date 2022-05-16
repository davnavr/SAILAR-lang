namespace SAILARSharp.Reader {
    using System;
    using SAILARSharp.Interop;

    public unsafe abstract class ModuleRecord : IDisposable {
        private OpaqueModuleRecord* record;
        private readonly RecordType recordType;
        private bool disposed = false;

        // Prevent users from deriving their own module record subclasses.
        private protected ModuleRecord(OpaqueModuleRecord* record, RecordType recordType) {
            this.record = record;
            this.recordType = recordType;
        }

        public static ModuleRecord Create(OpaqueModuleRecord* record) {
            return SAILAR.GetModuleRecordType(record) switch
            {
                RecordType.Identifier => new IdentifierRecord(record),
                RecordType.TypeSignature => new TypeSignatureRecord(record),
                RecordType.FunctionSignature => new FunctionSignatureRecord(record),
                RecordType.Data => new DataRecord(record),
                var invalid => throw new InvalidRecordTypeException(invalid),
            };
        }

        public RecordType GetRecordType() => recordType;

        private protected object Lock { get; } = new();

        public OpaqueModuleRecord* Reference => record;

        private protected void ThrowIfDisposed() {
            if (disposed) {
                throw new ObjectDisposedException(null);
            }
        }

        private protected virtual void Cleanup() { }

        public void Dispose() {
            lock (Lock) {
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
