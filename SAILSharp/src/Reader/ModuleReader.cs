namespace SAILSharp.Reader {
    using System;
    using System.Diagnostics;
    using SAILSharp;
    using SAILSharp.Interop;

    public unsafe sealed class ModuleReader : IDisposable {
        private OpaqueModuleReader* moduleReader;
        private OpaqueRecordReader* recordReader = default;
        private OpaqueBuffer* buffer = default;
        private readonly bool disposeMemoryBuffer = false;
        private bool disposed = false;

        private ModuleProperties moduleProperties = default;

        public ModuleReader(OpaqueBuffer* buffer, bool readerDisposesBuffer) {
            this.buffer = buffer;
            disposeMemoryBuffer = readerDisposesBuffer;
            moduleReader = SAILAR.CreateModuleReaderFromBuffer(buffer);
        }

        public ModuleReader(Interop.Buffer buffer, bool readerDisposesBuffer) : this(buffer.Reference, readerDisposesBuffer) {}

        public ModuleReader(byte[] bytes) : this(new Interop.Buffer(bytes), true) { }

        private bool IsRecordReaderInitialized => recordReader != null;

        private OpaqueRecordReader* GetRecordReader(in byte majorFormatVersion, in byte minorFormatVersion, in byte integerByteSize) {
            if (disposed) {
                throw new ObjectDisposedException(null);
            }

            if (!IsRecordReaderInitialized) {
                Debug.Assert(moduleReader != null);

                OpaqueError* error = default;
                fixed (byte* majorFormatVersionNumber = &majorFormatVersion, minorFormatVersionNumber = &minorFormatVersion, variableIntegerSize = &integerByteSize) {
                    recordReader = SAILAR.GetRecordsFromModuleReader(moduleReader, majorFormatVersionNumber, minorFormatVersionNumber, variableIntegerSize, &error);
                }

                Error.HandleError(error);
                moduleReader = null;
            }

            return recordReader;
        }

        private OpaqueRecordReader* GetRecordReader() {
            return GetRecordReader(in moduleProperties.majorFormatVersion, in moduleProperties.minorFormatVersion, in moduleProperties.integerByteSize);
        }

        public ModuleProperties GetModuleProperties() {
            if (!IsRecordReaderInitialized) {
                GetRecordReader();
            }

            return moduleProperties;
        }

        public void Dispose() {
            if (!disposed) {
                GC.SuppressFinalize(this);

                if (moduleReader == null) {
                    throw new NotImplementedException("How to dispose moduleReader if recordReader was not constructed?");
                }

                if (IsRecordReaderInitialized) {
                    SAILAR.DisposeRecordReader(recordReader);
                    recordReader = null;
                }

                if (disposeMemoryBuffer) {
                    new Interop.Buffer(buffer).Dispose();
                    buffer = default;
                }

                disposed = true;
            }
        }

        ~ModuleReader() {
            Dispose();
        }
    }
}
