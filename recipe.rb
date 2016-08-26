class wslient < FPM::Cookery::Recipe
      

      source "file://#{ENV['WORKSPACE']}"

      name     "#{ENV['PROJECT']}"
      version  "#{ENV['GIT_TAG']}"
      revision '1.0.0'

      description 'WebSocket Client Erlang application'

      provides "#{ENV['PROJECT']}"

      section 'application'

      post_install 'post-install'

      directories "#{ENV['INSTALL_DIR']}/#{name}/bin", "#{ENV['INSTALL_DIR']}/#{name}/include", "#{ENV['INSTALL_DIR']}/#{name}/lib", "#{ENV['INSTALL_DIR']}/#{name}/priv", "#{ENV['INSTALL_DIR']}/#{name}/releases"

      def build
        make
      end

      def install
        %w(_rel/wsclient/* priv include).each {|d| current_pathname_for("#{ENV['INSTALL_DIR']}/#{name}").install Dir[d]}
        current_pathname_for("#{ENV['INSTALL_DIR']}/#{name}/log").mkpath
        chmod 0750, current_pathname_for("#{ENV['INSTALL_DIR']}/#{name}/log")
      end
    end
