        Shiny.addCustomMessageHandler('toggleDrill', function(x){
          var leftTabs   = document.getElementById('model_analysis_tabs');
          var drillView  = document.getElementById('drillView');
          var metrics    = document.getElementById('metricsPanel');
          if (!leftTabs || !drillView || !metrics) return;
          if (x.show){
            leftTabs.classList.add('fade-out');
            drillView.classList.add('fade-in');
            metrics.classList.add('fade-in');
            leftTabs.classList.remove('fade-in'); drillView.classList.remove('fade-out'); metrics.classList.remove('fade-out');
          } else {
            leftTabs.classList.add('fade-in');
            drillView.classList.add('fade-out');
            metrics.classList.add('fade-out');
            leftTabs.classList.remove('fade-out'); drillView.classList.remove('fade-in'); metrics.classList.remove('fade-in');
          }
        });
        
          Shiny.addCustomMessageHandler('segmented-set-active', function(msg){
            var id = msg.id;
            var $btn = $('#' + id);
            var $wrap = $btn.closest('.segmented-control');
            $wrap.find('.btn').removeClass('active');
            $btn.addClass('active');
          });