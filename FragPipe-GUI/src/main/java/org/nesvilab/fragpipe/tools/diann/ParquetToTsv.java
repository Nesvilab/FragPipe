package org.nesvilab.fragpipe.tools.diann;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.apache.hadoop.fs.Path;
import org.apache.parquet.conf.ParquetConfiguration;
import org.apache.parquet.conf.PlainParquetConfiguration;
import org.apache.parquet.example.data.Group;
import org.apache.parquet.hadoop.ParquetReader;
import org.apache.parquet.hadoop.example.GroupReadSupport;

public class ParquetToTsv {

  public static void convertParquetToTsv(String inputParquetPath, String outputTsvPath) throws IOException {
    ParquetConfiguration configuration = new PlainParquetConfiguration();
    GroupReadSupport readSupport = new GroupReadSupport();

    try (ParquetReader<Group> reader = ParquetReader.builder(readSupport, new Path(inputParquetPath)).withConf(configuration).build();
        BufferedWriter writer = new BufferedWriter(new FileWriter(outputTsvPath))) {

      Group firstRecord = reader.read();
      if (firstRecord == null) {
        return;
      }
      int fieldCount = firstRecord.getType().getFieldCount();

      writer.write(IntStream.range(0, fieldCount).mapToObj(firstRecord.getType()::getFieldName).collect(Collectors.joining("\t")));
      writer.write("\n");

      writer.write(IntStream.range(0, fieldCount).mapToObj(i -> firstRecord.getValueToString(i, 0)).collect(Collectors.joining("\t")));
      writer.write("\n");

      Group record;
      while ((record = reader.read()) != null) {
        Group finalRecord = record;
        writer.write(IntStream.range(0, fieldCount).mapToObj(i -> finalRecord.getValueToString(i, 0)).collect(Collectors.joining("\t")));
        writer.write("\n");
      }
    }
  }


  public static void main(String[] args) {
    try {
      convertParquetToTsv(args[0], args[1]);
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
    }
  }
}
