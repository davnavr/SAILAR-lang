namespace SAILARSharp.Reader {
    using System;
    using System.Text;
    using SAILARSharp;
    using SAILARSharp.Interop;

    public unsafe sealed class ModuleReader : IDisposable {
        private OpaqueModuleReader* reader;
        private OpaqueBuffer* buffer = default;
        private ModuleFormat? format = null;

        private readonly object theLock = new();
        private readonly bool disposeMemoryBuffer = false;
        private bool disposed = false;

        private ModuleReader(OpaqueModuleReader* reader) {
            this.reader = reader;
        }

        public ModuleReader(OpaqueBuffer* buffer, bool readerDisposesBuffer) : this(SAILAR.CreateModuleReaderFromBuffer(buffer)) {
            this.buffer = buffer;
            disposeMemoryBuffer = readerDisposesBuffer;
        }

        public ModuleReader(ReadOnlySpan<byte> bytes) : this(Interop.Buffer.From(bytes), true) { }

        public ModuleReader(byte[] bytes) : this(new ReadOnlySpan<byte>(bytes)) { }

        private static OpaqueModuleReader* CreateReaderFromPath(string path) {
            byte[] pathBytes = Encoding.UTF8.GetBytes(path);
            OpaqueError* error;
            OpaqueModuleReader* reader;
            fixed (byte* pathAddress = pathBytes) {
                reader = SAILAR.CreateModuleReaderFromPath(pathAddress, (UIntPtr)path.Length, &error);
            }
            Error.HandleError(error);
            return reader;
        }

        /// <summary>
        /// Constructs a module reader from the specified path.
        /// </summary>
        public ModuleReader(string path) : this(CreateReaderFromPath(path)) { }

        public OpaqueModuleReader* Reference => reader;

        /// <summary>
        /// Gets or reads the module's format version and integer size.
        /// </summary>
        /// <exception cref="ErrorException">Thrown if an error occured while reading the module format.</exception>
        public ModuleFormat GetModuleFormat() {
            if (format == null) {
                OpaqueError* error;
                format = new ModuleFormat(SAILAR.ReadModuleFormat(reader, &error));
            }

            return format;
        }

        /// <summary>
        /// Reads the next record in the module.
        /// </summary>
        /// <returns>An object representing the parsed record, or <see langword="null"/> if the end of the module was reached.</returns>
        /// <exception cref="ErrorException">Thrown if an error occured while reading the record; or if the module format was not yet parsed with <see cref="GetModuleFormat"/>.</exception>
        public ModuleRecord? ReadNextRecord() {
            OpaqueError* error;
            var next = SAILAR.ReadModuleNextRecord(reader, &error);
            Error.HandleError(error);
            return next != null ? ModuleRecord.Create(next) : null;
        }

        /// <summary>
        /// Checks if the end of the module was reached.
        /// </summary>
        /// <exception cref="ErrorException">Thrown if the end of the module was not reached.</exception>
        public void CheckIfFinished() {
            OpaqueError* error;
            SAILAR.CheckModuleReaderFinished(reader, &error);
            Error.HandleError(error);
        }

        public void Dispose() {
            lock (theLock) {
                if (!disposed) {
                    GC.SuppressFinalize(this);

                    SAILAR.DisposeModuleReader(reader);
                    reader = null;

                    if (disposeMemoryBuffer) {
                        SAILAR.DisposeBuffer(buffer);
                        buffer = null;
                    }

                    disposed = true;
                }
            }
        }

        ~ModuleReader() {
            Dispose();
        }
    }
}
