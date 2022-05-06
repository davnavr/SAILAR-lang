namespace SAILSharp.Reader {
    public struct ModuleProperties {
        internal byte majorFormatVersion, minorFormatVersion, integerByteSize;

        public byte MajorFormatVersion => majorFormatVersion;

        public byte MinorFormatVersion => minorFormatVersion;

        /// <summary>
        /// Gets the size, in bytes, of the integers in a SAILAR module file.
        /// </summary>
        public byte IntegerByteSize => integerByteSize;
    }
}
