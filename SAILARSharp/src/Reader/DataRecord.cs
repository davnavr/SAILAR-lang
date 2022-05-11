namespace SAILARSharp.Reader {
    using System;
    using SAILARSharp.Interop;

    public unsafe sealed class DataRecord : ModuleRecord {
        private OpaqueBuffer* buffer;
        private byte* address;
        private int length;

        internal DataRecord(OpaqueModuleRecord* record) : base(record, RecordType.Data) {
            buffer = SAILAR.GetModuleRecordAsDataBuffer(record);
            UIntPtr length;
            address = SAILAR.GetBufferContents(buffer, &length);
            this.length = (int)length;
        }

        public ReadOnlySpan<byte> Content {
            get {
                lock (Lock) {
                    ThrowIfDisposed();
                    return new(address, length);
                }
            }
        }

        public byte[] ToArray() {
            return Content.ToArray();
        }

        private protected override void Cleanup() {
            SAILAR.DisposeBuffer(buffer);
            buffer = null;
            address = null;
            length = 0;
        }
    }
}
