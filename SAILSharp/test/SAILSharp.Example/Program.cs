namespace SAILSharp.Example {
    using SAILSharp.Reader;

    public class Program {
        public static void Main() {
            var contents = new byte[] { (byte)'S', (byte)'A', (byte)'I', (byte)'L', (byte)'A', (byte)'R', 0, 12, 0, 0 };
            using var reader = new ModuleReader(contents);
            System.Console.WriteLine(reader.GetModuleFormat().GetMinorFormatVersion());
        }
    }
}
