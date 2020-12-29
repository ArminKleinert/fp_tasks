class Files
  class << self

    def list_subs(start_path = "~", ignore_directories = false)
      fs = Dir[start_path.to_s + "/**/*"]
      fs.reject!{|f| f == '.' || f == '..'}
      fs.map!{ |f| File.absolute_path f }
      ignore_directories ? fs.reject {|f| File.directory? f} : fs
    end

    def map_files(start_path, ignore_directories = true, &block)
      fs = list_subs(start_path, ignore_directories)
      fs.map {|f| yield f}
    end

    def find_files_by(start_path, &block)
      fs = map_files(start_path, true) { |f| f if (yield f) }
      fs.filter { |f| !!f }
    end

    def filter_files_for_text(start_path, text, strict_casing=false, &pred)
      text = strict_casing ? text : text.downcase
      find_files_by(start_path) do |f|
        content = ""
        if yield(f)
          content = IO.read(f)
          begin
            content.downcase! unless strict_casing
          rescue
            content = ""
          end
        end
        content.include?(text)
      end
    end
  end
end

puts Files.filter_files_for_text(".", ARGV[0]) { |f| f.end_with? ".txt" or f.end_with? ".hs" }
